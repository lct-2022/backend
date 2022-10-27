(uiop:define-package #:passport/client
  (:use #:cl)
  (:import-from #:openrpc-client))
(in-package #:passport/client)


(openrpc-client:generate-client passport "http://localhost:8000/openrpc.json")

(defvar *client* (make-passport))

