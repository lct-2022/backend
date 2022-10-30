(uiop:define-package #:rating/client
  (:use #:cl)
  (:import-from #:openrpc-client)
  (:export #:connect
           #:make-rating))
(in-package #:rating/client)


(openrpc-client:generate-client rating "http://localhost:8002/openrpc.json")

(defvar *client* (make-rating))


(defun connect (client &optional token)
  (jsonrpc:client-connect client :mode :http :url "http://localhost:8002/"
                                 :headers (when token
                                            (list (cons :authorization
                                                        token))))
  client)
