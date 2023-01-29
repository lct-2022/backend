(uiop:define-package #:app/bus
  (:use #:cl)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:reblocks-websocket
                #:no-active-websockets)
  (:import-from #:trivial-garbage
                #:weak-pointer-value
                #:make-weak-pointer)
  (:import-from #:serapeum
                #:maybe-invoke-restart))
(in-package #:app/bus)


;; EVENTS: (:new-message message)

(defclass bus (event-emitter)
  ())


(defvar *bus* (make-instance 'bus))


(defun handler-already-added-p (event func)
  (let* ((listeners (event-emitter:listeners *bus* event)))
    (find func listeners
          :key #'event-emitter::listener-function)))


(defun add-event-handler (event func)
  (unless (handler-already-added-p event func)
    (event-emitter:on event *bus* func)))


(defun remove-event-handler (event func)
  (event-emitter:remove-listener *bus* event func))


(defmacro make-event-handler (func-name (&rest args) &body body)
  (flet ((let-bindings (&rest args)
           "Returns a list or given arguments while removing nils.
            Suitable to form let's bind in macroses."
           (remove-if #'null args)))
    (let* ((woo-package (find-package :woo))
           (ev-loop-symbol (when woo-package
                             (alexandria:ensure-symbol '*evloop*
                                                       :woo))))
      `(let* ,(let-bindings
               '(session reblocks/session::*session*)
               '(request reblocks/request::*request*
                 ;; (when (boundp 'reblocks/request::*request*)
                 ;;   reblocks/request::*request*)
                 )
               ;; Here we are using weak pages because all event handlers
               ;; are created inside particular page and should be removed
               ;; from the bus when page gets expired:
               '(page (make-weak-pointer reblocks/page::*current-page*))
               '(app reblocks/variables::*current-app*)
               (when ev-loop-symbol
                 (list 'evloop ev-loop-symbol)))
         ;; Here we need to drop this header if it exists,
         ;; to make ajax-request-p return false for subsequent calls
         ;; in the thread.
         (when (reblocks/request:get-header "X-Requested-With"
                                            :request request)
           (setf request
                 (reblocks/request:remove-header "X-Requested-With"
                                                 :request request)))

         (flet ((,func-name (,@args)
                  (let ,(let-bindings
                         '(reblocks/session::*session* session)
                         '(reblocks/request::*request* request)
                         '(reblocks/page::*current-page* (weak-pointer-value page))
                         '(reblocks/variables::*current-app* app)
                         ;; Hack
                         (when woo-package
                           (list ev-loop-symbol 'evloop))
                         '(reblocks-websocket::*background* t))
                    (cond
                      (reblocks/page::*current-page*
                       ,@body)
                      ;; when page was expired, remove listener
                      (t
                       (let ((restart (find-restart 'wsd:remove-listener)))
                         (cond
                           (restart
                            ;; Removing listener because it's page was expired.
                            (invoke-restart restart))
                           (t
                            ;; We should never get here
                            (log:warn "No restart for removing listener.")))))))))
           #',func-name)))))


(defun emit-with-continue (event &rest args)
  (handler-bind ((no-active-websockets
                   (lambda (condition)
                     (let ((restart (find-restart 'wsd:remove-listener condition)))
                       (when restart
                         (log:warn "Removing listener because no active websockets.")
                         (invoke-restart restart)))))
                 (error (lambda (condition)
                          (log4cl-extras/error:print-backtrace :condition condition)
                          (maybe-invoke-restart 'continue))))
    (apply #'event-emitter:emit event *bus* args)))


(defun send-message-to-the-bus (message)
  (handler-bind ((error (lambda (condition)
                          (log4cl-extras/error:print-backtrace :condition condition)
                          (let ((restart (find-restart 'continue condition)))
                            (when restart
                              (invoke-restart restart))))))
    (event-emitter:emit :new-message *bus* message)))
