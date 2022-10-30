(uiop:define-package #:passport/client
  (:use #:cl)
  (:import-from #:openrpc-client)
  (:import-from #:common/rpc
                #:cached-url-as))
(in-package #:passport/client)


(openrpc-client:generate-client passport
                                (cached-url-as "http://localhost:8000/openrpc.json"
                                               (asdf:system-relative-pathname :common "specs/passport.json")))

(defvar *client* (make-passport))


(defun connect (client &optional token)
  (jsonrpc:client-connect client :mode :http :url "http://localhost:8000/"
                                 :headers (when token
                                            (list (cons :authorization
                                                        token)))))
