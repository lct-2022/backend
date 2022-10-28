(uiop:define-package #:platform/team/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/team/model
                #:team)
  (:import-from #:common/db
                #:with-connection))
(in-package #:platform/team/api)


(define-rpc-method (platform-api create-team) (project-id title)
  (:param title string)
  (:param project-id integer)
  (:result team)

  (with-connection ()
    (mito:create-dao 'team
                     :title title
                     :project-id project-id)))


(define-rpc-method (platform-api get-project-teams) (project-id)
  (:param project-id integer)
  (:result (list-of team))

  (with-connection ()
    (mito:select-dao 'team
      ;; Сейчас это не работает для get-project-team - проект не раскрывается CL клиентом, но на фронте должно быть OK.
      ;; Пока не нужно. поэтому закомментировано
      ;; (mito:includes 'platform/project/model::project)
      (sxql:where (:= :project-id project-id)))))
