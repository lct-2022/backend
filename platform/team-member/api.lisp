(uiop:define-package #:platform/team-member/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/team-member/model
                #:team-member)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:platform/job/api
                #:create-job)
  (:import-from #:mito.dao
                #:select-by-sql))
(in-package #:platform/team-member/api)


(define-rpc-method (platform-api get-team-members) (project-id)
  (:summary "Отдаёт список участников команды.")
  (:description "Поскольку у нас микросервисы, то профили пользователей надо запросить отдельно через passport.get-profiles.")
  (:param project-id integer)
  (:result (list-of team-member))

  (with-connection ()
    (select-by-sql 'team-member
                   "SELECT t.*
                      FROM platform.team_member as t
                      JOIN platform.job AS j ON t.job_id = j.id
                      JOIN platform.team AS tm ON j.team_id = tm.id
                     WHERE tm.project_id = ?"
                   :binds (list project-id))))
