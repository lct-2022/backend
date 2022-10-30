(uiop:define-package #:chat/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api))
(in-package #:chat/api)


(openrpc-server:define-api (chat-api :title "Chat API"))
