(uiop:define-package #:chat/client
  (:use #:cl)
  (:import-from #:openrpc-client))
(in-package #:chat/client)


(openrpc-client:generate-client chat-api "http://localhost:8003/openrpc.json")

(defvar *client* (make-chat-api))

(defun connect (client &optional token)
  (jsonrpc:client-connect client :mode :http :url "http://localhost:8003/"
                                 :headers (when token
                                            (list (cons :authorization
                                                        token))))
  (values client))

