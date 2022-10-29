(uiop:define-package #:common/search
  (:use #:cl #:common/utils)
  (:import-from #:jonathan)
  (:import-from #:log)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:import-from #:serapeum
                #:dict
                #:fmt)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:yason
                #:with-output-to-string*)
  (:import-from #:alexandria
                #:lastcar
                #:length=
                #:ensure-list))
(in-package #:common/search)


(defun get-elastic-host ()
  (or (uiop:getenv "ELASTIC_HOST")
      "192.168.0.103"))

(defun get-elastic-port ()
  (or (uiop:getenv "ELASTIC_PORT")
      "9200"))

(defun get-elastic-user ()
  (or (uiop:getenv "ELASTIC_USER")
      "admin"))

(defun get-elastic-password ()
  (uiop:getenv "ELASTIC_PASS"))


(defun index (collection id data)
  (let ((content (jonathan:to-json data))
        (url (fmt "https://~A:~A/~A/_doc/~A"
                  (get-elastic-host)
                  (get-elastic-port)
                  collection
                  (quri:url-encode id))))
    (log:info "Sending data to Elastic Search" collection id)
    (jonathan:parse
     (dex:put url
              :content content
              :headers '(("Content-Type" . "application/json"))
              :insecure t
              :basic-auth (cons (get-elastic-user)
                                (get-elastic-password))))))

(defun check-analyzer (analyzer text)
  (let ((content (jonathan:to-json (dict "analyzer" analyzer
                                         "text" text)))
        (url (fmt "https://~A:~A/_analyze"
                  (get-elastic-host)
                  (get-elastic-port))))
    (log:info "Checking ~S analyzer" analyzer)
    (jonathan:parse
     (dex:post url
               :content content
               :headers '(("Content-Type" . "application/json"))
               :insecure t
               :basic-auth (cons (get-elastic-user)
                                 (get-elastic-password))))))


(defun create-index (index)
  "Создаёт Elastic индекс с включенной русской морфологией."
  (let ((content (jonathan:to-json
                  (dict "settings"
                        (dict "analysis"
                              (dict "analyzer"
                                    (dict "default"
                                          (dict "type" "russian")))))))
        (url (fmt "https://~A:~A/~A"
                  (get-elastic-host)
                  (get-elastic-port)
                  index)))
    (jonathan:parse
     (dex:put url
              :content content
              :headers '(("Content-Type" . "application/json"))
              :insecure t
              :basic-auth (cons (get-elastic-user)
                                (get-elastic-password))))))

(defun create-indices ()
  (create-index "projects")
  (create-index "users")
  (create-index "jobs")
  (values))


(defun delete-index (collection)
  (let ((url (fmt "https://~A:~A/~A"
                  (get-elastic-host)
                  (get-elastic-port)
                  collection)))
    (log:info "Deleting index" collection)
    (jonathan:parse
     (dex:delete url
                 :headers '(("Content-Type" . "application/json"))
                 :insecure t
                 :basic-auth (cons (get-elastic-user)
                                   (get-elastic-password))))))


(defun delete-from-index (collection doc-id)
  (with-fields (:document-id doc-id)
    (let ((url (fmt "https://~A:~A/~A/_doc/~A"
                    (get-elastic-host)
                    (get-elastic-port)
                    collection
                    (quri:url-encode doc-id))))
      (log:info "Deleting document from index" collection)
      (jonathan:parse
       (dex:delete url
                   :headers '(("Content-Type" . "application/json"))
                   :insecure t
                   :basic-auth (cons (get-elastic-user)
                                     (get-elastic-password)))))))


(define-condition bad-query (error)
  ((original-error :initarg :original-error
                   :reader get-original-error)))


(defun make-sort-param (plist)
  "Делает из (list :created-at :desc :title :asc) список словарей:
   (list (dict \"created-at\" \"desc\")
         (dict \"title\" \"asc\"))
   "
  (loop for (key value) on plist by #'cddr
        collect (dict (string-downcase key)
                      (string-downcase value))))


(defun extract-next-page-key (sort-plist dict)
  "Достаёт из словаря элементы по ключам из параметров сортировки."
  (loop for key in (serapeum:plist-keys sort-plist)
        collect (gethash (string-downcase key) dict)))


(defun search-objects (collection term &key
                                         (sort (list :created-at :desc))
                                         (limit 10)
                                         (page-key nil))
  ;; TODO: научиться обрабатывать 400 ответы от Elastic
  ;; например на запрос: TYPE:macro AND storage NAME:FLEXI-STREAMS:WITH-OUTPUT-TO-SEQUENCE
  (handler-case
      (loop with url = (fmt "https://~A:~A/~A/_search"
                            (get-elastic-host)
                            (get-elastic-port)
                            collection)
            with sort-param = (make-sort-param sort)
            with query = (dict "query" (dict
                                        "query_string"
                                        (dict "fields"
                                              (list "title" "description")
                                              "query" term))
                               ;; Сортировка используется для keyset пейджинации
                               ;; и подгрузки результатов:
                               ;; https://www.elastic.co/guide/en/elasticsearch/reference/current/paginate-search-results.html
                               "sort" sort-param
                               "size" limit)
            with content = (with-output-to-string* ()
                             (when page-key
                               (setf (gethash "search_after" query)
                                     (ensure-list page-key)))
                             (yason:encode query))
            with body = (dex:post url
                                  :content content
                                  :headers '(("Content-Type" . "application/json"))
                                  :insecure t
                                  :basic-auth (cons (get-elastic-user)
                                                    (get-elastic-password)))
            with response = (yason:parse body)
            with total = (el response "hits/total/value")
            for hit in (el response "hits/hits")
            for id = (el hit "_id")
            for source = (el hit "_source")
            for doc = (el source "documentation")
            collect source into results
            finally (return (if (length= limit results)
                                ;; Возможно есть следующая страница
                                (values results
                                        total
                                        (extract-next-page-key sort (lastcar results)))
                                (values results
                                        total))))
    (dexador.error:http-request-not-found ()
      (values nil 0 nil))
    (dexador.error:http-request-bad-request (condition)
      (error 'bad-query :original-error condition))))
