(uiop:define-package #:rating/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:common/server)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:openrpc-server/api
                #:define-api)
  (:import-from #:rating/api
                #:rating-api)
  (:import-from #:rating/vote/api))
(in-package #:rating/server)


(defvar *default-port* 8002)


(defun start-me (&key (port *default-port*)
                   (interface "localhost"))
  (common/server::start rating-api port
                        :interface interface))


(defun stop-me (&key (port *default-port*))
  (common/server::stop port))
