(uiop:define-package #:rating/vote/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:rating/api
                #:rating-api)
  (:import-from #:rating/vote/model
                #:top-item
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


(define-rpc-method (rating-api get-ratings) (subject-type subject-ids)
  (:summary "Возвращает рейтинги объектов.")
  (:param subject-type string "Тип объекта: project или user.")
  (:param subject-ids (list-of integer) "ID объектов.")
  (:result (list-of integer))

  (check-subject-type subject-type)

  (with-connection ()
    (loop with rows = (when subject-ids
                        (retrieve-by-sql
                         (sxql:select (:subject_id (:as (:raw "COUNT(*)")
                                                    :rating))
                           (sxql:from :rating.vote)
                           (sxql:where (:and (:= :subject_type subject-type)
                                             (:in :subject_id subject-ids)))
                           (sxql:group-by :subject_id))))
          with map = (make-hash-table :test 'equal)
          for row in rows
          do (setf (gethash (getf row :subject-id) map)
                   (getf row :rating))
          finally (return
                    (mapcar (lambda (id)
                              (gethash id map 0))
                            subject-ids)))))


(define-rpc-method (rating-api get-top) (subject-type &key (limit 10))
  (:summary "Возвращает N объектов с наивысшим рейтингом в своей категории.")
  (:param subject-type string "Тип объекта: project или user.")
  (:param limit integer "Максимальное количество объектов в топе. По умолчанию 10.")
  (:result (list-of top-item))

  (check-subject-type subject-type)

  (with-connection ()
    (select-dao 'top-item
      (where (:= :subject-type subject-type))
      (order-by (:desc :rating))
      (limit limit))))
