(uiop:define-package #:rating/client
  (:use #:cl)
  (:import-from #:openrpc-client))
(in-package #:rating/client)


(openrpc-client:generate-client rating "http://localhost:8002/openrpc.json")

(defvar *client* (make-rating))
