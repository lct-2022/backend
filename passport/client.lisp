(uiop:define-package #:passport/client
  (:use #:cl)
  (:import-from #:openrpc-client))
(in-package #:passport/client)


(openrpc-client:generate-client passport "http://localhost:8000/openrpc.json")

(defvar *client* (make-passport))


(defun connect (&optional token)
  (jsonrpc:client-connect *client* :mode :http :url "http://localhost:8000/"
                                   :headers (when token
                                              (list (cons :authorization
                                                          token)))))
