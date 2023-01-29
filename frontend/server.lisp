(uiop:define-package #:app/server
  (:use #:cl)
  (:import-from #:common/logging)
  (:import-from #:log)
  (:import-from #:reblocks/request)
  (:import-from #:reblocks/server)
  (:import-from #:common/slynk
                #:start-slynk-if-needed)
  (:import-from #:app/app
                #:app)
  (:import-from #:app/vars
                #:*inside-form*
                #:*url-prefix*)
  (:import-from #:reblocks/request-handler
                #:remove-action-from-uri)
  (:import-from #:reblocks/variables)
  (:import-from #:app/processes
                #:ensure-every-minute-thread-is-running)
  (:import-from #:app/controllers/programme-workflow
                #:schedule-regular-workflow-update)
  (:shadow #:restart)
  (:export
   #:restart
   #:start
   #:stop))
(in-package #:app/server)


(defvar *arguments* nil)


;; Это хак нужен чтобы в формах формировались правильные пути
(defun reblocks/request:get-path (&key (request reblocks/request::*request*) with-params)
  "For URL http://example.com/foo/bar?blah=minor returns
/foo/bar path of the request's URL."
  (let ((result
          (if with-params
              ;; request-uri returns path-info + GET params
              (lack.request::request-uri request)
              ;; Otherwice, return only a path
              (lack.request::request-path-info request))))
    (if *inside-form*
        (concatenate 'string
                     *url-prefix*
                     result)
        result)))

(defun reblocks/request-handler::handle-action-if-needed (app)
  (let ((action-name (reblocks/request::get-action-name-from-request))
        (action-arguments
          (reblocks/utils/list::alist->plist (reblocks/request::get-parameters))))

    (when action-name
      (log:debug "Processing action" action-name)

      ;; Remove :action key from action arguments
      (remf action-arguments (alexandria:make-keyword (string-upcase reblocks/variables::*action-string*)))

      (multiple-value-bind (action-result current-page)
          (reblocks/hooks:with-action-hook (app action-name action-arguments)
            (reblocks/actions::eval-action
             app
             action-name
             action-arguments))
      
        (cond
          ((reblocks/request::pure-request-p)
           (log:debug "Request is pure, processing will be aborted.")
           (reblocks/response::immediate-response action-result))
          ;; Remove "action" parameter for the GET parameters
          ;; if it is not an AJAX request
          ((not (reblocks/request::ajax-request-p))
           (let ((url (remove-action-from-uri
                       (reblocks/request::get-path :with-params t))))
             (log:debug "Redirecting to an URL without action parameter" url)
             (reblocks/response::redirect (concatenate 'string
                                                       *url-prefix*
                                                       url))))
          (t
           (values action-result
                   current-page)))))))


;; Этот хак нужен, чтобы правильно работало формирование урлов внутри Reblocks
;; если с помощью проксирования в Nginx сайт отдаётся не из корня, а с урла /alternative/
(defvar *initializing* nil)

(defmethod initialize-instance :around ((obj reblocks/dependencies::dependency) &rest args)
  (declare (ignore args))
  (let ((*initializing* t))
    (call-next-method)))


(defmethod reblocks/dependencies:get-url :around ((dependency t))
  (let ((url (call-next-method)))
    (cond
      ((and (not *initializing*)
            (str:starts-with-p "/" url))
       (concatenate 'string
                    *url-prefix*
                    url))
      (t
       url))))


(defun setup-gc ()
  (setf (sb-ext:bytes-consed-between-gcs)
        (* 10 1024 1024))
  
  (loop for gen upto 6
        do (setf (sb-ext:generation-bytes-consed-between-gcs gen)
                 (* 5 1024 1024)))
  
  (loop for gen upto 6
        do (setf (sb-ext:generation-minimum-age-before-gc gen)
                 0.75d0))

  (if (and (uiop:getenv "PORT")
           (string-equal (uiop:getenv "PORT") "9011"))
      (setf (sb-ext:gc-logfile)
            ;; With 0.25 min age
            "/tmp/gc.log2")
      (setf (sb-ext:gc-logfile)
            ;; With 0.25 min age
            "/tmp/gc-api.log2")))


(defun start (&rest args
              &key
              (port (parse-integer
                     (or (uiop:getenv "PORT")
                         "9001")))
	      (interface "localhost")
              (debug nil))
  (when (probe-file ".local-config.lisp")
    (load (probe-file ".local-config.lisp")))
  
  (setup-gc)
  
  ;; Just to suppres debug logs to TTY from Reblocks.
  ;; I'll need to fix Reblocks to prohibit it from
  ;; configure logging if they are already configured.
  (common/logging::setup)
  (start-slynk-if-needed)
  (local-time:reread-timezone-repository)

  ;; TODO: может быть это и не нужно?
  (cl+ssl:ssl-load-global-verify-locations #P"/home/art/.postgresql/root.crt")

  (setf reblocks/variables:*pages-expire-in* (* 10 60))
  (setf reblocks/variables:*max-pages-per-session* 10)

  (ensure-every-minute-thread-is-running)
  (schedule-regular-workflow-update)
  
  (reblocks/server:start :port port
			 :interface interface
                         :apps 'app
                         ;; WOO requires libev, and also
                         ;; there is a problem with hanging workers:
                         ;; https://github.com/fukamachi/woo/issues/100
			 :server-type :hunchentoot
                         :debug debug)
  (common/logging::setup)
  (log:info "Server started")
  (setf *arguments* args)
  ;; Чтобы не выскакивал дебаггер
  (setf reblocks/variables:*ignore-missing-actions* t)
  (values))


(defun cl-user::start-server ()
  ;; Entry point for webapp, started in the Docker
  (start :port (parse-integer (or (uiop:getenv "APP_PORT")
				  "9001"))
	 :interface (or (uiop:getenv "APP_INTERFACE")
			"0.0.0.0"))
  (loop do (sleep 5)))


(defun stop ()
  (reblocks/server:stop (getf *arguments* :interface)
                        (getf *arguments* :port))
  (values))


(defun restart ()
  (stop)
  (apply #'start *arguments*)
  (with-simple-restart (ignore "Ignore and continue")
    (reblocks/debug:reset-latest-session))
  (values))
