(uiop:define-package #:rating/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api))
(in-package #:rating/api)


(define-api (rating-api :title "Rating API"))
