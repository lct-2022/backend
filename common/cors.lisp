(uiop:define-package #:common/cors
  (:use #:cl))
(in-package #:common/cors)


(defun process-cors-middleware (env app access-control)
  (let ((path (getf env :path-info)))
    (log:info "Processing request with" path))
  
  (destructuring-bind (code headers content)
      (funcall app env)
    (list code
          (if (member :Access-Control-Allow-Origin headers)
              headers
              (list* :Access-Control-Allow-Origin access-control
                     headers))
          content)))


(defun make-cors-middleware (app &key (access-control "*"))
  (lambda (env)
    (process-cors-middleware env app access-control)))

