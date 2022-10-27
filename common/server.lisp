(uiop:define-package #:common/server
  (:use #:cl)
  (:import-from #:clack
                #:clackup)
  (:import-from #:openrpc-server
                #:make-clack-app))
(in-package #:common/server)


(defvar *servers* (make-hash-table :test 'equal))

(defun find-server (port interface)
  (gethash (cons port interface)
           *servers*))

(defun register-server (port interface server)
  (setf (gethash (cons port interface)
                 *servers*)
        server))

(defun delete-server (port interface)
  (remhash (cons port interface)
           *servers*))

(defun start (port &key (interface "localhost"))
  (when (find-server port interface)
    (error "Server already running"))
  
  (register-server port interface
                   (clackup (make-clack-app :indent-json t)
                            :address interface
                            :port port)))

(defun stop (port &key (interface "localhost"))
  (let ((server (find-server port interface)))
    (when server
      (clack:stop server)
      (delete-server port interface))))
