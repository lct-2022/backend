(uiop:define-package #:platform/team-member/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/team-member/model
                #:team-member)
  (:import-from #:platform/project/model
                #:project)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:platform/job/api
                #:create-job)
  (:import-from #:mito.dao
                #:select-by-sql))
(in-package #:platform/team-member/api)


(define-rpc-method (platform-api get-team-members) (&key project-id team-id)
  (:summary "Отдаёт список участников проекта (из всех команд) или команды.")
  (:description "Поскольку у нас микросервисы, то профили пользователей надо запросить отдельно через passport.get-profiles.")
  (:param project-id integer)
  (:param team-id integer)
  (:result (list-of team-member))

  (when (and (not project-id)
             (not team-id))
    (return-error "Надо указать либо project-id либо team-id"))

  (with-connection ()
    (if project-id
        (select-by-sql 'team-member
                       "SELECT t.*
                      FROM platform.team_member as t
                      JOIN platform.job AS j ON t.job_id = j.id
                      JOIN platform.team AS tm ON j.team_id = tm.id
                     WHERE tm.project_id = ?"
                       :binds (list project-id))
        (select-by-sql 'team-member
                       "SELECT t.*
                      FROM platform.team_member as t
                      JOIN platform.job AS j ON t.job_id = j.id
                     WHERE j.team_id = ?"
                       :binds (list team-id)))))


(define-rpc-method (platform-api user-projects) (user-id)
  (:summary "Отдаёт список проектов, в которых пользователь участник команды или автор")
  (:description "Поскольку у нас микросервисы, то профили пользователей надо запросить отдельно через passport.get-profiles.")
  (:param user-id integer)
  (:result (list-of project))

  (with-connection ()
    (select-by-sql 'project
                   "SELECT p.*
                      FROM platform.project as p
                     WHERE p.author_id = ?
                     UNION
                    SELECT p.*
                      FROM platform.project as p
                      JOIN platform.team as t ON p.id = t.project_id
                      JOIN platform.job AS j ON t.id = j.team_id
                      JOIN platform.team_member as tm ON j.id = tm.job_id
                     WHERE tm.user_id = ?"
                   :binds (list user-id
                                user-id))))
