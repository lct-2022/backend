(uiop:define-package #:passport/search
  (:use #:cl
        #:common/utils)
  (:import-from #:passport/user
                #:user-skills
                #:user-skill-ids
                #:user-profession-id
                #:user-profession
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
                #:define-search-rpc-method)
  (:import-from #:str
                #:join))
(in-package #:passport/search)


(defmethod make-document-for-index ((user user))
  (uiop:symbol-call "PASSPORT/SERVER" "ENRICH-USER" user nil)
  
  (dict "fio" (user-fio user)
        "country" (user-country user)
        "city" (user-city user)
        "education" (user-education user)
        "profession" (user-profession user)
        "profession_id" (user-profession-id user)
        "skills" (join ", " (user-skills user))
        "skill_ids" (user-skill-ids user)
        "profession_id" (user-profession-id user)
        "job" (user-job user)
        "about" (user-about user)))


(defmethod get-fields-to-search ((user (eql 'user)))
  (list "fio" "country" "city" "education" "job" "about"
        "profession" "skills"))


(defun enrich (users fields)
  (uiop:symbol-call "PASSPORT/SERVER" "ENRICH-USERS" users fields))


(define-search-rpc-method (passport-api search-users user :enrich-func #'enrich)
  (:summary "Возвращает список пользователей по заданному запросу.")
  (:description "Запрос должен вводиться в формате ElasticSearch. Но для поиска по всем
 полям можно просто слова вводить.  Если передать \"*\" - выдаются все пользователи, начиная с самых свежих.

Можно давать сложные запросы типа city: Moscow AND country: Russia.
Ещё, можно использовать такие поля как profession и skills. Например:

    profession: backend AND skills: agile

Этот метод поддерживает пейджинацию и можно запрашивать следующие страницы результатов."))
