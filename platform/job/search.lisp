(uiop:define-package #:platform/job/search
  (:use #:cl
        #:common/utils)
  (:import-from #:platform/job/model
                #:job-title
                #:job-description
                #:job)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:common/search
                #:get-objects-to-index
                #:make-document-for-index
                #:define-search-rpc-method))
(in-package #:platform/job/search)


(defmethod make-document-for-index ((job job))
  (dict "title" (job-title job)
        "description" (job-description job)))


(defmethod get-objects-to-index ((class-name (eql 'job)))
  ;; В случае с вакансиями, нам надо отдавать только открытые
  (mito:retrieve-dao class-name
                     :open t))


(define-search-rpc-method (platform-api search-jobs job)
  (:summary "Возвращает список вакансий по заданному запросу.")
  (:description "Запрос должен вводиться в формате ElasticSearch. Но для поиска по всем
 полям можно просто слова вводить.  Если передать \"*\" - выдаются все джобы, начиная с самых свежих.

Этот метод поддерживает пейджинацию и можно запрашивать следующие страницы результатов."))
