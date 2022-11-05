(uiop:define-package #:common/csv
  (:use #:cl)
  ;; using https://github.com/massung/csv
  (:import-from #:csv)
  (:export
   #:parse-csv))
(in-package #:common/csv)


(defun parse-csv (filename &key (separator ";"))
  (when (probe-file filename)
    (with-open-file (s filename)
      (csv:read-csv s (csv:make-csv-format :separator separator)))))
