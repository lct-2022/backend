(uiop:define-package #:app/vars
  (:use #:cl)
  (:import-from #:function-cache
                #:defcached)
  (:export
   #:*current-source-id*))
(in-package #:app/vars)


(defparameter *url-prefix* "")

(defvar *inside-form* nil)


(defparameter *dark-background* "rgb(51, 53, 65)")

(defparameter *light-background* "rgb(61, 63, 75)")

(defparameter *text-color* "rgb(235, 236, 241)")


(defparameter *current-source-id* 2)
