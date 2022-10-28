(uiop:define-package #:common/session
  (:use #:cl)
  (:import-from #:lack.request
                #:request-headers)
  (:import-from #:openrpc-server/vars
                #:return-error
                #:*current-request*)
  (:import-from #:common/token
                #:decode)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:alexandria
                #:ensure-list
                #:make-keyword
                #:with-gensyms))
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


(defmacro with-session (((&rest bindings) &key (require t))
                        &body body)
  (with-gensyms (session-var)
    (let ((bindings
            (loop for var in (ensure-list bindings)
                  for key = (string-downcase var)
                  collect (cond
                            ((string= key "roles")
                             `(,var (loop for role in (gethash "roles" ,session-var)
                                          collect (make-keyword (string-upcase role)))))
                            (t
                             `(,var (gethash ,key ,session-var)))))))
      `(let* ((,session-var (decode-current-jwt-token))
              ,@bindings)
         (when (and ,require
                    (not ,session-var))
           (return-error "Этот метод требует аутентификации."
                         :code 3))
         ,@body))))
