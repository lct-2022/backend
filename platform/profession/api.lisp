(uiop:define-package #:platform/profession/api
  (:use #:cl
        #:common/utils)
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
                #:assoc-value)
  (:import-from #:platform/profession/model
                #:profession))
(in-package #:platform/profession/api)


(define-rpc-method (platform-api get-professions) ()
  (:summary "Возвращает список всех профессий, какие могут быть у экспертов.")
  (:description "ID этих объектов можно использовать, чтобы изменить в профиле пользователя его profession-id.")
  (:result (list-of profession))
  
  (with-connection ()
    (retrieve-dao 'profession)))
