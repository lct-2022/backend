(uiop:define-package #:passport/search
  (:use #:cl
        #:common/utils)
  (:import-from #:passport/user
                #:user-nickname
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
  
  (dict "nickname" (user-nickname user)))


(defmethod get-fields-to-search ((user (eql 'user)))
  (list "nickname"))


(defun enrich (users fields)
  (uiop:symbol-call "PASSPORT/SERVER" "ENRICH-USERS" users fields))


(define-search-rpc-method (passport-api search-users user :enrich-func #'enrich)
  (:summary "Возвращает список пользователей по заданному запросу.")
  (:description "Запрос должен вводиться в формате ElasticSearch. Но для поиска по всем
 полям можно просто слова вводить.  Если передать \"*\" - выдаются все пользователи, начиная с самых свежих.

Можно давать сложные запросы типа city: Moscow AND country: Russia.
Ещё, можно использовать такие поля как profession и skills. Например:

    profession: backend AND skills: agile

Или если удобнее, то можно передавать айдишники навыков или профессии:

    profession_id: 7 AND skill_ids: 42

Этот метод поддерживает пейджинацию и можно запрашивать следующие страницы результатов."))
