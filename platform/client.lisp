(uiop:define-package #:platform/client
  (:use #:cl)
  (:import-from #:openrpc-client)
  (:import-from #:common/rpc
                #:cached-url-as))
(in-package #:platform/client)


(openrpc-client:generate-client platform
                                (cached-url-as "http://localhost:8001/openrpc.json"
                                               (asdf:system-relative-pathname :common "specs/platform.json")))

(defvar *client* (make-platform))

(defun connect (client &optional token)
  (jsonrpc:client-connect client :mode :http :url "http://localhost:8001/"
                                 :headers (when token
                                            (list (cons :authorization
                                                        token))))
  (values client))

