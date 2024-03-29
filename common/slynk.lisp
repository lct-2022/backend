(defpackage #:common/slynk
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:global-vars
                #:define-global-var)

  (:import-from #:slynk)
  (:import-from #:slynk-macrostep)
  (:import-from #:slynk-named-readtables)
  ;; TODO: разобраться почему загружается непатченный slynk
  ;; из #P"/home/art/.roswell/lisp/sly/git/slynk/"
  ;; (:import-from #:slynk-package-inferred)
  
  (:export #:*connections*
           #:start-slynk-if-neede))
(in-package #:common/slynk)


(define-global-var *connections* nil
  "Here we'll store all Slynk connections.")

;; We need to define this variable before slynk will load
;; mrepl plugin.
(defvar slynk:*use-dedicated-output-stream*)

(defvar *slynk-already-running* nil)


(defun on-connection-open (conn)
  (log:info "SLY connected")
  (push conn *connections*))


(defun on-connection-close (conn)
  (log:info "SLY disconnected")
  (setf *connections*
        (remove conn *connections*)))


(defun start-slynk-if-needed ()
  (when (and (uiop:getenv "SLYNK_PORT")
             (not *slynk-already-running*))
    (let ((port (parse-integer (uiop:getenv "SLYNK_PORT")))
	  (interface (or (uiop:getenv "SLYNK_INTERFACE")
			 "127.0.0.1"))
	  (hostname (machine-instance)))
      (slynk-api:add-hook slynk-api:*new-connection-hook*
			  'on-connection-open)
      (slynk-api:add-hook slynk-api:*connection-closed-hook*
			  'on-connection-close)
      (setf slynk:*use-dedicated-output-stream* nil)
      (slynk:create-server :dont-close t
                           :port port
                           :interface interface)
      (format t "Run ssh -6 -L ~A:localhost:4005 ~A~%"
              port
              hostname)
      (format t "Then open local Emacs and connect to the slynk on 4005 port~%")
      
      (setf *slynk-already-running* t)))
  (values))
