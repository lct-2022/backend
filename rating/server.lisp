(uiop:define-package #:rating/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:sha1
                #:sha1-hex)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:openrpc-server/api
                #:define-api)
  (:import-from #:rating/api
                #:rating-api)
  ;; 
  ;; (:import-from #:rating/job/api)
  )
(in-package #:rating/server)



(defun start-me ()
  (common/server::start rating-api 8002))

(defun stop-me ()
  (common/server::stop 8002))
