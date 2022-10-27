(uiop:define-package #:common/session
  (:use #:cl)
  (:import-from #:lack.request
                #:request-headers)
  (:import-from #:openrpc-server
                #:return-error/vars
                #:*current-request*)
  (:import-from #:common/token
                #:decode)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled))
(in-package #:common/session)


(defun decode-current-jwt-token ()
  (let* ((headers (request-headers *current-request*))
         (token (gethash "authorization" headers)))
    (when token
      (handler-case
          (with-log-unhandled ()
            (decode token))
        (error (c)
          (openrpc-server:return-error (format nil "Невозможно распарсить Authorization токен: ~A"
                                               c)))))))


(defmacro with-session ((var &key (require t)) &body body)
  `(let ((,var (decode-current-jwt-token)))
     (when (and ,require
                (not ,var))
       (return-error "Этот метод требует аутентификации."
                     :code 3))
     ,@body))
