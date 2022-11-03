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
                #:platform-api)
  (:import-from #:platform/job/api)
  (:import-from #:platform/job/search)
  (:import-from #:platform/team/api)
  (:import-from #:platform/project/api)
  (:import-from #:platform/project/search)
  (:import-from #:platform/job-application/api)
  (:import-from #:platform/team-member/api))
(in-package #:platform/server)



(defun start-me ()
  (common/server::start platform-api 8001))

(defun stop-me ()
  (common/server::stop 8001))
