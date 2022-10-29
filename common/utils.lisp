(uiop:define-package #:common/utils
  (:use #:cl)
  (:import-from #:cl-json-pointer
                #:get-by-json-pointer)
  (:import-from #:serapeum
                #:dict)
  (:export #:el
           #:dict))
(in-package #:common/utils)


(defun el (hash path)
  (let ((path (if (char= (elt path 0) #\/)
                  path
                  (concatenate 'string "/"
                               path))))
    (get-by-json-pointer hash path :flavor :yason)))
