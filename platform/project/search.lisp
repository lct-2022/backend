(uiop:define-package #:platform/project/search
  (:use #:cl
        #:common/utils)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:platform/project/model
                #:project-title
                #:project-description
                #:project-contests
                #:project)
  (:import-from #:mito
                #:object-id)
  (:import-from #:serapeum
                #:fmt
                #:dict)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:common/search
                #:search-objects)
  (:import-from #:yason
                #:with-output-to-string*))
(in-package #:platform/project/search)


(defun index-project (project)
  (common/search::index "projects"
                        (fmt "~A" (object-id project))
                        (dict "id" (object-id project)
                              "title" (project-title project)
                              "description" (project-description project)
                              "contests" (project-contests project)
                              "created-at" (local-time:timestamp-to-unix (mito:object-created-at project)))))


(defun index ()
  "Индексируем все проекты.
   Для прода надо будет сделать какой-то pipeline, чтобы добавлять в индекс только новые или обновлённые."
  (with-connection ()
    (loop for project in (mito:retrieve-dao 'project)
          do (index-project project))))


(define-rpc-method (platform-api search-projects) (query &key (limit 10) page-key)
  (:param query string "Запрос для поиска на языке запросов ElasticSearch. Если передать \"*\" - выдаются все проекты, начиная с самых свежих.")
  (:param limit integer)
  (:param page-key string)
  (:result (paginated-list-of project))

  (when page-key
    (setf page-key
          (decode-json page-key)))
  
  (multiple-value-bind (search-results total next-page-key)
      (search-objects "projects" query :limit limit)
    (declare (ignore total))
    (let* ((ids (loop for result in search-results
                      collect (el result "id")))
           (results (when ids
                      (with-connection ()
                        (mito:select-dao 'project
                          (where (:in :id ids)))))))
      (if next-page-key
          (values results
                  (encode-json next-page-key))
          results))))


