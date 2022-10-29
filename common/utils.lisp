(uiop:define-package #:common/utils
  (:use #:cl)
  (:import-from #:cl-json-pointer
                #:get-by-json-pointer)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:yason
                #:with-output-to-string*)
  (:import-from #:mito
                #:select-dao
                #:retrieve-dao
                #:retrieve-by-sql
                #:find-dao)
  (:import-from #:sxql
                #:where
                #:order-by
                #:limit)
  (:import-from #:common/db
                #:with-connection)
  (:export #:el
           #:dict
           #:encode-json
           #:decode-json
           ;; Полезные символы чтобы делать SQL запросы
           #:with-connection
           #:select-dao
           #:retrieve-dao
           #:retrieve-by-sql
           #:find-dao
           #:where
           #:order-by
           #:limit))
(in-package #:common/utils)


(defun el (hash path)
  (let ((path (if (char= (elt path 0) #\/)
                  path
                  (concatenate 'string "/"
                               path))))
    (get-by-json-pointer hash path :flavor :yason)))


(defun encode-json (obj)
  (with-output-to-string* ()
    (yason:encode obj)))


(defun decode-json (obj)
  (yason:parse obj))
