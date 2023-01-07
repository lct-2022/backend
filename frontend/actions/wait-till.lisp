(uiop:define-package #:app/actions/wait-till
  (:use #:cl)
  (:import-from #:org.shirakumo.fraf.action-list
                #:time-limited-action)
  (:import-from #:org.shirakumo.fraf.action-list
                #:lane-limited-action)
  (:import-from #:serapeum/bundle
                #:required-argument)
  (:import-from #:org.shirakumo.fraf.action-list
                #:blocking-p)
  (:import-from #:org.shirakumo.fraf.action-list
                #:update)
  (:import-from #:org.shirakumo.fraf.action-list
                #:clone-into)
  (:import-from #:org.shirakumo.fraf.action-list
                #:finished-p)
  (:import-from #:org.shirakumo.fraf.action-list
                #:lanes)
  (:import-from #:local-time
                #:timestamp<))
(in-package #:app/actions/wait-till)


(defclass wait-till (lane-limited-action)
  ((time :initarg :time
         :initform (required-argument :time)
         :type local-time:timestamp
         :accessor wait-till-time)
   (lanes :initform (1- (ash 1 32)))))



(defmethod blocking-p ((action lane-limited-action))
  t)


(defmethod update ((action wait-till) dt))

(defmethod update :after ((action wait-till) dt)
  (declare (ignore dt))
  (when (timestamp< (wait-till-time action) (local-time:now))
    (setf (finished-p action) T)))


(defmethod clone-into progn ((new wait-till) (action wait-till))
  (setf (wait-till-time new)
        (wait-till-time action)))
