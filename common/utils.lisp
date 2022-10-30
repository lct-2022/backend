(uiop:define-package #:common/utils
  (:use #:cl)
  (:import-from #:cl-json-pointer
                #:get-by-json-pointer)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:uuid)
  (:import-from #:yason
                #:with-output-to-string*)
  (:import-from #:mito
                #:object-id
                #:create-dao
                #:select-dao
                #:retrieve-dao
                #:retrieve-by-sql
                #:find-dao)
  (:import-from #:mito.dao
                #:select-by-sql)
  (:import-from #:sxql
                #:where
                #:order-by
                #:limit)
  (:import-from #:common/db
                #:with-connection
                #:select-dao-by-ids)
  (:export #:el
           #:dict
           #:encode-json
           #:decode-json
           ;; Полезные символы чтобы делать SQL запросы
           #:object-id
           #:create-dao
           #:with-connection
           #:select-dao
           #:select-dao-by-ids
           #:retrieve-dao
           #:retrieve-by-sql
           #:select-by-sql
           #:find-dao
           #:where
           #:order-by
           #:limit
           #:make-uuid))
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


(defun make-uuid ()
  (format nil "~(~A~)"
          (uuid:make-v4-uuid)))
