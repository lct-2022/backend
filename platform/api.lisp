(uiop:define-package #:platform/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api))
(in-package #:platform/api)


(openrpc-server:define-api (platform-api :title "Platform API"))
