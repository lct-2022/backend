(uiop:define-package #:platform/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:sha1
                #:sha1-hex)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:openrpc-server/api
                #:define-api)
  (:import-from #:platform/api
                #:platform-api))
(in-package #:platform/server)



(defun start-me ()
  (common/server::start platform-api 8001))

(defun stop-me ()
  (common/server::stop 8001))
