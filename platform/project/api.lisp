(uiop:define-package #:platform/project/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/project/model
                #:project)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:mito
                #:find-dao
                #:select-dao
                #:object-id)
  (:import-from #:platform/team/api
                #:create-team)
  (:import-from #:sxql
                #:order-by
                #:limit)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:common/rpc
                #:define-update-method)
  (:import-from #:common/event-bus
                #:emit-event))
(in-package #:platform/project/api)


(define-rpc-method (platform-api create-project) (title description &rest rest &key url contests)
  (:summary "Создаёт проект, одну команду под него, и наполняет команду примером вакансий.")
  (:param title string)
  (:param description string)
  (:param url string)
  (:param contests string)
  (:result project)
  (declare (ignore url contests))

  (with-session (user-id)
    (with-connection ()
      (let* ((project (apply #'mito:create-dao
                             'project
                             :author-id user-id
                             :title title
                             :description description
                             rest)))
        (create-team (object-id project)
                     "Команда проекта")
        project))))


(define-rpc-method (platform-api get-project) (id)
  (:summary "Возвращает описание проекта по его id.")
  (:param id integer)
  (:result project)
  
  (with-connection ()
    ;; Пока тупо выдаём все, позже будем делать запрос в систему рейтингов
    ;; и потом запрашивать по ID из своей базы.
    (let ((project (find-dao 'project :id id)))
      (if project
          project
          (return-error (fmt "Проект с id = ~A не найден." id))))))


(define-update-method (platform-api update-project project)
                      (id title description url contests)
  (unless id
    (return-error "Параметр ID обязательный."
                  :code 5))
  
  (find-dao 'project
            :id id))


(define-rpc-method (platform-api popular-projects) (&key (limit 5))
  (:summary "Отдаёт список популярных проектов для главной страницы.")
  (:param limit integer)
  (:result (list-of project))
  
  (with-connection ()
    ;; Пока тупо выдаём все, позже будем делать запрос в систему рейтингов
    ;; и потом запрашивать по ID из своей базы.
    (or (select-dao 'project
          (order-by (:desc :created-at))
          (limit limit))
        #())))
