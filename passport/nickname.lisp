(uiop:define-package #:passport/nickname
  (:use #:cl))
(in-package #:passport/nickname)


(defvar *nicknames-pool* nil)


(defun get-random-nicknames ()
  (let* ((response (dex:post "https://randomus.ru/nickname"
                             :content '(("action" . "generate")
                                        ("min_len" . "4")
                                        ("max_len" . "10")
                                        ("first_letter" . "-1"))))
         (document (plump:parse response)))
    (loop for item across (clss:select ".result_element" document)
          collect (string-downcase (str:trim (plump:text item))))))


(defun fill-pool-if-needed ()
  (unless *nicknames-pool*
    (setf *nicknames-pool*
          (get-random-nicknames))))


(defun take-name-from-the-pool ()
  (prog1 (car *nicknames-pool*)
    (setf *nicknames-pool*
          (cdr *nicknames-pool*))))


(defun get-random-nickname ()
  (fill-pool-if-needed)
  (take-name-from-the-pool))
