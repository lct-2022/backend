(uiop:define-package #:platform/job-application/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/job/model
                #:job-open-p
                #:job-title
                #:job)
  (:import-from #:platform/job-application/model
                #:application-job
                #:application-user-id
                #:application-status
                #:job-application)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:sxql
                #:order-by
                #:limit
                #:where)
  (:import-from #:mito
                #:find-dao
                #:save-dao
                #:create-dao
                #:select-dao)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:common/permissions
                #:assert-can-modify)
  (:import-from #:platform/team/model
                #:get-team-id)
  (:import-from #:platform/team-member/model
                #:team-member)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:mito.dao
                #:select-by-sql))
(in-package #:platform/job-application/api)


(defmethod get-team-id ((obj job-application))
  (get-team-id
   (mito:find-dao 'job
                  :id (application-job obj))))


(define-rpc-method (platform-api get-job-application) (job-id)
  (:summary "Возвращает последний отклик текущего пользователя на указанную вакансию.")
  (:description "Так можно получить статус отклика.
Если этот метод что-то отдаёт, то не нужно показывать пользователю кнопку “Откликнуться”.")
  (:param job-id integer)
  (:result job-application)

  (with-session (user-id)
    (with-connection ()
      (first
       (select-dao 'job-application
         (where (:and (:= :job-id job-id)
                      (:= :user-id user-id)) )
         (order-by (:desc :created-at))
         (limit 1))))))


(define-rpc-method (platform-api apply-to-job) (job-id &optional (message "Хочу к вам в команду!"))
  (:summary "Добавляет отклик на вакансию в команде от текущего пользователя.")
  (:description "Если у пользователя уже есть незавершенный отклик на эту вакансию, то метод вернёт его вместо нового объекта.")
  (:param job-id integer)
  (:param message string)
  (:result job-application)

  (with-session (user-id)
    (with-connection ()
      (let ((existing (get-job-application job-id)))
        (cond
          ((and existing
                ;; Если отклик пока не приняли и не отклонили,
                ;; отдадим его
                (string-equal (application-status existing)
                              "applied"))
           existing)
          ;; Иначе - создадим новый
          (t
           (create-dao 'job-application
                       :job-id job-id
                       :user-id user-id
                       :message message)))))))


(define-rpc-method (platform-api accept-application) (id)
  (:summary "Подтверждает отклик и добавляет участника в команду проекта.")
  (:description "Подтвердить участие может только владелец проекта или админ.")
  (:param id integer "ID отклика на вакансию")
  (:result job-application)

  (with-session (user-id)
    (with-connection ()
      (let ((existing (find-dao 'job-application :id id)))
        (when existing
          ;; Проверим права
          (assert-can-modify user-id existing)

          (unless (string-equal (application-status existing)
                                "applied")
            (return-error (fmt "Принять можно только отклик в статусе \"applied\", а у этого ~S."
                               (application-status existing))))

          ;; Поменяем статус
          (setf (application-status existing)
                "accepted")
          (save-dao existing)
          
          ;; Пометим вакансию закрытой
          (let ((job (application-job existing)))
            (when job
              (setf (job-open-p job) nil)
              (save-dao job)))

          ;; Добавим пользователя в команду
          (let ((user-id (application-user-id existing))
                (job (application-job existing)))
            (mito:create-dao 'team-member
                             :user-id user-id
                             :job job
                             :title (job-title job)))
          ;; Вернём "отклик"
          existing)))))


(define-rpc-method (platform-api decline-application) (id)
  (:summary "Отклоняет отклик.")
  (:description "Отклонить может только только владелец проекта или админ.")
  (:param id integer "ID отклика на вакансию")
  (:result job-application)

  (with-session (user-id)
    (with-connection ()
      (let ((existing (find-dao 'job-application :id id)))
        (when existing
          ;; Проверим права
          (assert-can-modify user-id existing)

          (unless (string-equal (application-status existing)
                                "applied")
            (return-error (fmt "Отклонить можно только отклик в статусе \"applied\", а у этого ~S."
                               (application-status existing))))

          ;; Поменяем статус
          (setf (application-status existing)
                "declined")
          (save-dao existing)

          ;; Вернём "отклик"
          existing)))))


(define-rpc-method (platform-api get-job-applications) (project-id)
  (:summary "Отдаёт список открытых вакансий на проекте.")
  (:param project-id integer)
  (:result (list-of job-application))

  (with-connection ()
    (select-by-sql 'job-application
                   "SELECT ja.*
                      FROM platform.job_application AS ja
                      JOIN platform.job AS j ON ja.job_id = j.id
                      JOIN platform.team AS tm ON j.team_id = tm.id
                     WHERE tm.project_id = ? AND j.open"
                   :binds (list project-id))))
