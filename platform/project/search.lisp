(uiop:define-package #:platform/project/search
  (:use #:cl)
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
                #:dict))
(in-package #:platform/project/search)


(defun index-project (project)
  (common/search::index "projects"
                        (fmt "~A" (object-id project))
                        (dict "title" (project-title project)
                              "description" (project-description project)
                              "contests" (project-contests project)
                              "created-at" (local-time:timestamp-to-unix (mito:object-created-at project)))))


(defun index ()
  "Индексируем все проекты.
   Для прода надо будет сделать какой-то pipeline, чтобы добавлять в индекс только новые или обновлённые."
  (with-connection ()
    (loop for project in (mito:retrieve-dao 'project)
          do (index-project project))))


