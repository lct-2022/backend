(uiop:define-package #:platform/project/search
  (:use #:cl
        #:common/utils)
  (:import-from #:platform/project/model
                #:project-title
                #:project-description
                #:project-contests
                #:project)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:common/search
                #:make-document-for-index
                #:define-search-rpc-method))
(in-package #:platform/project/search)


(defmethod make-document-for-index ((project project))
  (dict "title" (project-title project)
        "description" (project-description project)
        "contests" (project-contests project)))


(define-search-rpc-method (platform-api search-projects project)
  (:summary "Возвращает список проектов по заданному запросу.")
  (:description "Запрос должен вводиться в формате ElasticSearch. Но для поиска по всем
 полям можно просто слова вводить.  Если передать \"*\" - выдаются все проекты, начиная с самых свежих.

Этот метод поддерживает пейджинацию и можно запрашивать следующие страницы результатов."))
