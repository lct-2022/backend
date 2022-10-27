(uiop:define-package #:common/cors
  (:use #:cl))
(in-package #:common/cors)


(defun process-cors-middleware (env app access-control allowed-headers)
  (destructuring-bind (code headers content)
      (funcall app env)
    (list code
          (append
           (unless (member :Access-Control-Allow-Origin headers)
             (list :Access-Control-Allow-Origin access-control))
           (list :Access-Control-Allow-Headers allowed-headers)
           headers)
          content)))


(defun make-cors-middleware (app &key (access-control "*")
                                   (allowed-headers "Authorization"))
  (lambda (env)
    (process-cors-middleware env app access-control allowed-headers)))

