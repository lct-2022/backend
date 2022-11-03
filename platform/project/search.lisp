(uiop:define-package #:platform/project/search
  (:use #:cl
        #:common/utils)
  (:import-from #:platform/project/model
                #:project-stage-id
                #:project-title
                #:project-description
                #:project-contests
                #:project)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:common/search
                #:make-document-for-index
                #:define-search-rpc-method)
  (:import-from #:platform/project/api
                #:enrich-projects)
  (:import-from #:platform/stage/model
                #:get-stage-title))
(in-package #:platform/project/search)


(defmethod make-document-for-index ((project project))
  (dict "title" (project-title project)
        "description" (project-description project)
        "stage_id" (project-stage-id project)
        "stage" (get-stage-title (project-stage-id project))
        "contests" (project-contests project)))


(define-search-rpc-method (platform-api search-projects project :enrich-func 'enrich-projects)
  (:summary "Возвращает список проектов по заданному запросу.")
  (:description "Запрос должен вводиться в формате ElasticSearch. Но для поиска по всем
 полям можно просто слова вводить.  Если передать \"*\" - выдаются все проекты, начиная с самых свежих.

Этот метод поддерживает пейджинацию и можно запрашивать следующие страницы результатов.

Для поиска проектов в определённой стадии, надо добавить оказание этапа к запросу. Например так:
\"метро AND stage_id: 3\" можно найти все проекты упоминающие метро и находящиеся в стадии \"Проработка бизнес-плана\".
"))
