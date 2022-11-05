(uiop:define-package #:platform/project/api
  (:use #:cl
        #:common/utils)
  (:import-from #:common/csv
                #:parse-csv)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/project/model
                #:project-stage-title
                #:project-stage-id
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
                #:*test-token*
                #:with-session)
  (:import-from #:serapeum
                #:random-in-range
                #:dict
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
                #:assoc-value)
  (:import-from #:platform/stage/model
                #:get-stage-title)
  (:import-from #:random-sample
                #:random-sample))
(in-package #:platform/project/api)


(define-rpc-method (platform-api create-project) (title description &rest rest &key url contests industry innovation-type innovations)
  (:summary "Создаёт проект, одну команду под него, и наполняет команду примером вакансий.")
  (:param title string)
  (:param description string)
  (:param url string)
  (:param contests string)
  (:param industry string "Индустрия в которой значим проект.")
  (:param innovation-type string "Тип инновации.")
  (:param innovations string "Более подробное описание инновации")
  (:result project)
  (declare (ignore url contests))

  (with-session (user-id)
    (with-connection ()
      (let* ((project (apply #'mito:create-dao
                             'project
                             :author-id user-id
                             :title title
                             :description description
                             :industry industry
                             :innovation-type innovation-type
                             :innovations innovations
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
                             0)))))
      ;; Заполним названия этапов проектов
      (loop for project in projects
            do (setf (project-stage-title project)
                     (get-stage-title (project-stage-id project))))))
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
                      (id
                       title
                       description
                       url
                       contests
                       industry
                       innovation-type
                       innovations
                       stage-id)
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


;; TODO: Уже нет времени вносить такие мелкие словари в БД. Но по хорошему - надо потом переделать.
(defparameter *industries*
  (list "Здравоохранение и медицина"
        "Образование"
        "Производство"
        "Рестораны и питание"
        "Сельское хозяйство"
        "Сервисы неселению"
        "Социальная"
        "Товары"
        "Торговля и ecommerce"
        "Транспорт и перевозки"))


(define-rpc-method (platform-api get-industries) ()
  (:summary "Возвращает список строк, которые можно подставлять в поле industry проекта.")
  (:result (list-of string))
  *industries*)


(defparameter *innovation-types*
  (list "Автоматизция процессов"
        "Нестандартный подход"
        "Нет инновации"
        "Новая бизнес-модель"
        "Новая технология"
        "Социальная"
        "Уникальный сервис"
        "Уникальный товар"))


(define-rpc-method (platform-api get-innovation-types) ()
  (:summary "Возвращает список строк, которые можно подставлять в поле innovation-type проекта.")
  (:result (list-of string))
  *innovation-types*)


(defclass project-stats ()
  ((num-projects :initarg :num-projects
                 :type integer
                 :documentation "Количество проектов в базе.")
   (supported-projects :initarg :supported-projects
                       :type integer
                       :documentation "Количество проектов получивших поддержку (с голосами).")
   (num-jobs :initarg :num-jobs
             :type integer
             :documentation "Количество открытых вакансий.")))


(define-rpc-method (platform-api get-stats) ()
  (:summary "Возвращает словарь со статистикой по проектам.")
  (:result project-stats)
  (flet ((sql (query)
           (getf (first (mito.db:retrieve-by-sql query))
                 :cnt)))
    (with-connection ()
      (make-instance 'project-stats
                     :num-projects (sql "SELECT COUNT(*) as cnt FROM platform.project")
                     :supported-projects (sql "SELECT COUNT(DISTINCT subject_id) as cnt FROM rating.vote WHERE subject_type = 'project'")
                     :num-jobs (sql "SELECT COUNT(open) as cnt FROM platform.job")))))


(defun random-author-id (&key (starting-from 38))
  (with-connection ()
    (loop for row in (mito:retrieve-by-sql "select id from passport.user where id > ?"
                                           :binds (list starting-from))
          collect (getf row :id) into ids
          finally (return (car (random-sample ids 1))))))


(defun get-n-random-project-ids (n &key (starting-from 1))
  (with-connection ()
    (random-sample
     (loop for row in (mito:retrieve-by-sql "select id from platform.project where id >= ?"
                                            :binds (list starting-from))
           collect (getf row :id))
     n
     :with-replacement nil)))


(defun load-from-csv ()
  (let* ((lines
           (loop for row in (nthcdr 12 (parse-csv "~/DataSet.csv"))
                 unless (alexandria:length= 1 row)
                   collect (cdr row)))
         (header (mapcar #'string-downcase
                         (first lines)))
         (data (rest lines)))
    (loop for line in data
          for dict = (alexandria:alist-hash-table
                      (mapcar #'cons header line)
                      :test 'equal)
          for author-id = (random-author-id)
          for common/session::*test-token* = (dict "user-id" author-id)
          for title = (gethash "название" dict)
          do (log:info "Adding project with title ~S" title)
             (create-project title
                             (gethash "описание" dict)
                             :industry (gethash "индустрия" dict)
                             :innovation-type (gethash "тип инновации" dict)
                             :innovations (gethash "описание новшества" dict)))))


(defun add-random-votes ()
  (loop for project-id in (get-n-random-project-ids 10 :starting-from 68)
        for num-votes = (random-in-range 3 10)
        do (loop repeat num-votes
                 for author-id = (random-author-id)
                 for *test-token* = (dict "user-id" author-id)
                 do (uiop:symbol-call :rating/vote/api :vote
                                      "project" project-id))))
