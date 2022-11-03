(uiop:define-package #:platform/project/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/project/model
                #:project-team-size
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
                #:where
                #:join
                #:from
                #:select
                #:order-by
                #:limit)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:serapeum
                #:~>
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
                #:select-by-sql)
  (:import-from #:group-by
                #:group-by)
  (:import-from #:alexandria
                #:assoc-value))
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


(defun enrich-projects (projects fields)
  (with-connection ()
    (let ((ids (mapcar #'object-id projects)))
      (when (member "jobs" fields :test #'string-equal)
        (let* ((rows
                 (retrieve-by-sql
                  (select (:t.project_id :j.*)
                    (from (:as :platform.job :j))
                    (join (:as :platform.team :t)
                          :on (:= :j.team_id :t.id))
                    (where (:and (:in :t.project_id ids)
                            :j.open)))))
               (id-to-jobs (group-by rows
                                     :key (lambda (item)
                                            (~> item (getf :project-id)))
                                     :value (lambda (item)
                                              (apply #'mito:make-dao-instance
                                                     'job (cddr item))))))
          (loop for project in projects
                do (setf (project-jobs project)
                         (assoc-value id-to-jobs
                                      (object-id project))))))
      
      (when (member "team-size" fields :test #'string-equal)
        (let* ((rows
                 (retrieve-by-sql
                  (select (:t.project_id (:as (:raw "count(*)") :team_size))
                    (from (:as :platform.team_member :tm))
                    (join (:as :platform.job :j)
                               :on (:= :tm.job_id :j.id))
                    (join (:as :platform.team :t)
                               :on (:= :j.team_id :t.id))
                    (where (:and (:in :t.project_id ids)))
                    (sxql:group-by :t.project_id))))
               (id-to-counters (loop for row in rows
                                     collect (cons (getf row :project-id)
                                                   (getf row :team-size)))))
          (loop for project in projects
                do (setf (project-team-size project)
                         (or (assoc-value id-to-counters
                                          (object-id project))
                             0)))))))
  (values projects))


(defun enrich-project (project fields)
  (enrich-projects (list project) fields)
  project)


(define-rpc-method (platform-api get-project) (id &key additional-fields)
  (:summary "Возвращает описание проекта по его id.")
  (:description "В additional-fields можно передать список из \"jobs\" и/или \"team-size\",
                 чтобы в результате были заполнены эти поля.")
  (:param additional-fields (list-of string)
          "Этот список может содержать имена полей \"jobs\" или \"team-size\".")
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

                 В additional-fields можно передать список из \"jobs\" и/или \"team-size\",
                 чтобы в результате были заполнены эти поля.")
  (:param additional-fields (list-of string)
          "Этот список может содержать имена полей \"jobs\" или \"team-size\".")
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
