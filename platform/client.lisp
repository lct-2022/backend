(uiop:define-package #:platform/client
  (:use #:cl)
  (:import-from #:openrpc-client))
(in-package #:platform/client)


(openrpc-client:generate-client platform "http://localhost:8001/openrpc.json")

(defvar *client* (make-platform))

(defun connect ()
  (jsonrpc:client-connect *client* :mode :http :url "http://localhost:8001/"))

