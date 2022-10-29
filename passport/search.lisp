(uiop:define-package #:passport/search
  (:use #:cl
        #:common/utils)
  (:import-from #:passport/user
                #:user
                #:user-fio
                #:user-country
                #:user-city
                #:user-education
                #:user-job
                #:user-about)
  (:import-from #:passport/server
                #:passport-api)
  (:import-from #:common/search
                #:get-fields-to-search
                #:get-objects-to-index
                #:make-document-for-index
                #:define-search-rpc-method))
(in-package #:passport/search)


(defmethod make-document-for-index ((user user))
  (dict "fio" (user-fio user)
        "country" (user-country user)
        "city" (user-city user)
        "education" (user-education user)
        "job" (user-job user)
        "about" (user-about user)))


(defmethod get-fields-to-search ((user (eql 'user)))
  (list "fio" "country" "city" "education" "job" "about"))


(define-search-rpc-method (passport-api search-users user)
  (:summary "Возвращает список пользователей по заданному запросу.")
  (:description "Запрос должен вводиться в формате ElasticSearch. Но для поиска по всем
 полям можно просто слова вводить.  Если передать \"*\" - выдаются все пользователи, начиная с самых свежих.

Можно давать сложные запросы типа city: Moscow AND country: Russia.

Этот метод поддерживает пейджинацию и можно запрашивать следующие страницы результатов."))
