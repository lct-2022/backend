(uiop:define-package #:common/cors
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:serapeum
                #:fmt))
(in-package #:common/cors)


(defun process-cors-middleware (env app access-control allowed-headers)
  (let ((response (funcall app env)))
    (cond
      ((length= 3 response)
       (destructuring-bind (code headers content)
           response
         (list code
               (append
                (unless (member :Access-Control-Allow-Origin headers)
                  (list :Access-Control-Allow-Origin access-control))
                (list :Access-Control-Allow-Headers allowed-headers)
                headers)
               content)))
      (t
       (log:error "Something strange, I've got response with wrong number of items" response env)
       (list 500
             (list :Content-Type "application/json")
             (list "{\"code\": -1, \"message\": \"Unhandled error.\"}"))))))


(defun make-cors-middleware (app &key (access-control "*")
                                   (allowed-headers "Authorization"))
  (lambda (env)
    (process-cors-middleware env app access-control allowed-headers)))

