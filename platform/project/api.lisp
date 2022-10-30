(uiop:define-package #:platform/project/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/project/model
                #:project
                #:project-with-rating)
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
                #:emit-event)
  (:import-from #:rating/client
                #:make-rating))
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
  (:description "Отдаёт не просто проекты, а структуру, содержащую и данные проекта и рейтинг.")
  (:param limit integer)
  (:result (list-of project-with-rating))
  
  (with-connection ()
    ;; Сначала стучимся с микросервис рейтингов, и получаем top популярных проектов
    (let* ((client (rating/client:connect (make-rating)))
           (top (rating/client::get-top client "project" :limit limit))
           (top-ids (mapcar #'rating/client::top-item-subject-id top))
           (projects
             ;; Теперь запросим их по id и отдадим уже проекты
             (select-dao-by-ids 'project
                                top-ids)))
      ;; Нам надо вернуть объект вместе с его рейтингом.
      (loop for top-item in top
            for project in projects
            collect (make-instance 'project-with-rating
                                   :project project
                                   :rating (rating/client::top-item-rating top-item))))))
