(uiop:define-package #:platform/project/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/project/model
                #:project-jobs
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
                #:make-rating)
  (:import-from #:chat/client
                #:chat-id
                #:create-chat
                #:make-chat-api)
  (:import-from #:platform/project-chat/model
                #:project-chat)
  (:import-from #:platform/job/model
                #:job)
  (:import-from #:mito.dao
                #:select-by-sql))
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
                             rest))
             (team (create-team (object-id project)
                                "Команда проекта"))
             (chat-api (chat/client::connect (make-chat-api)))
             (public-chat (create-chat chat-api
                                       :team-id (object-id team)))
             (private-chat (create-chat chat-api
                                        :team-id (object-id team)
                                        :private t)))
        (create-dao 'project-chat
                    :project project
                    :chat-id (chat-id public-chat))
        (create-dao 'project-chat
                    :project project
                    :chat-id (chat-id private-chat)
                    :private t)
        (values project)))))


(defun enrich-project (project fields)
  ;; TODO: Не совсем оптимально делать подзапросы для каждого проекта,
  ;; но пока так проще. В будущем надо будет сделать балковый enrich
  (with-connection ()
    (when (member "jobs" fields :test #'string-equal)
      (setf (project-jobs project)
            (select-by-sql 'job
                           "SELECT j.*
                      FROM platform.job as j
                      JOIN platform.team as t ON j.team_id = t.id
                     WHERE t.project_id = ? AND j.open"
                           :binds (list (object-id project))))))
  (values project))


(define-rpc-method (platform-api get-project) (id &key additional-fields)
  (:summary "Возвращает описание проекта по его id.")
  (:description "В additional-fields можно передать список из \"jobs\" и/или \"team\",
                 чтобы в результате были заполнены эти поля.")
  (:param additional-fields (list-of string))
  (:param id integer)
  (:result project)
  
  (with-connection ()
    (let ((project (find-dao 'project :id id)))
      (if project
          (enrich-project project additional-fields)
          (return-error (fmt "Проект с id = ~A не найден." id))))))


(define-update-method (platform-api update-project project)
                      (id title description url contests)
  (unless id
    (return-error "Параметр ID обязательный."
                  :code 5))
  
  (find-dao 'project
            :id id))

(define-rpc-method (platform-api popular-projects) (&key (limit 5) additional-fields)
  (:summary "Отдаёт список популярных проектов для главной страницы.")
  (:description "Отдаёт не просто проекты, а структуру, содержащую и данные проекта и рейтинг.

                 В additional-fields можно передать список из \"jobs\" и/или \"team\",
                 чтобы в результате были заполнены эти поля.")
  (:param additional-fields (list-of string))
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
                                   :project (enrich-project project additional-fields)
                                   :rating (rating/client::top-item-rating top-item))))))


(define-rpc-method (platform-api project-chats) (project-id)
  (:summary "Возвращает список чатов привязанных к проекту.")
  (:description "Дальше уже по chat-id можно запрашивать из микросервиса чатов сообщения от пользователей.")
  (:param project-id integer)
  (:result (list-of project-chat))
  
  (with-connection ()
    (values
     (retrieve-dao 'project-chat :project-id project-id))))
