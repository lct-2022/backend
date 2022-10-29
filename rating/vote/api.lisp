(uiop:define-package #:rating/vote/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:rating/api
                #:rating-api)
  (:import-from #:rating/vote/model
                #:*supported-subject-types*
                #:vote)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:mito
                #:retrieve-by-sql
                #:find-dao
                #:create-dao))
(in-package #:rating/vote/api)


(defun check-subject-type (subject-type)
  (unless (member subject-type *supported-subject-types* :test #'string-equal)
    (return-error (fmt "Subject type ~S is not supported. Use one of ~{~S~^, ~}."
                       subject-type
                       *supported-subject-types*))))


(define-rpc-method (rating-api vote) (subject-type subject-id)
  (:summary "Оставляет голос от текущего пользователя за объект с указанным типом и id.")
  (:param subject-type string "Тип объекта: project или user.")
  (:param subject-id integer "ID объекта.")
  (:result vote)

  (check-subject-type subject-type)
  
  (with-session (user-id)
    (with-connection ()
      (or (find-dao 'vote
                    :user-id user-id
                    :subject-type subject-type
                    :subject-id subject-id)
          (create-dao 'vote
                      :user-id user-id
                      :subject-type subject-type
                      :subject-id subject-id)))))


(define-rpc-method (rating-api get-vote) (subject-type subject-id)
  (:summary "Возвращает объект Vote, если текущий пользователь уже голосовал за объект.")
  (:param subject-type string "Тип объекта: project или user.")
  (:param subject-id integer "ID объекта.")
  (:result vote)

  (check-subject-type subject-type)
  
  (with-session (user-id)
    (with-connection ()
      (find-dao 'vote
                :user-id user-id
                :subject-type subject-type
                :subject-id subject-id))))


(define-rpc-method (rating-api get-rating) (subject-type subject-id)
  (:summary "Возвращает рейтинг объекта (количество голосов за него).")
  (:param subject-type string "Тип объекта: project или user.")
  (:param subject-id integer "ID объекта.")
  (:result integer)

  (check-subject-type subject-type)

  (with-connection ()
    (let ((rows (retrieve-by-sql "SELECT COUNT(*) as count FROM rating.vote WHERE subject_type = ? AND subject_id = ?"
                                 :binds (list subject-type subject-id))))
      (getf (first rows) :count))))
