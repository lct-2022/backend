(uiop:define-package #:common/rpc
  (:use #:cl)
  (:import-from #:closer-mop
                #:slot-definition-type
                #:slot-definition-name
                #:class-slots
                #:ensure-finalized)
  (:import-from #:alexandria
                #:write-string-into-file
                #:symbolicate)
  (:import-from #:dexador)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:common/permissions
                #:assert-can-modify))
(in-package #:common/rpc)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slots-with-types (class-name)
    (let* ((class (ensure-finalized (find-class class-name)))
           (slots (class-slots class)))
      (loop for slot in slots
            for name = (slot-definition-name slot)
            for type = (slot-definition-type slot)
            collect (list name type)))))


(defmacro define-update-method ((api method-name model) fields &body body)
  (let* ((types (slots-with-types model))
         (positional-args (loop for name in fields
                                when (string-equal name "id")
                                  collect name))
         (fields-kwargs (loop for name in fields
                              for given-name = (symbolicate name "-GIVEN-P")
                              unless (string-equal name "id")
                                collect (list name nil given-name)))
         (update-code (loop for (name default given-name) in fields-kwargs
                            ;; In the TYPES alist, first item is a symbol,
                            ;; corresponding to the slot name
                            for slot-name = (first (assoc name types :test #'string-equal))
                            ;; TODO: use accessors here instead of slot-value
                            collect `(when ,given-name
                                       (setf (slot-value object ',slot-name)
                                             ,name))))
         (param-definitions (loop for name in fields
                                  for type = (second (assoc name types :test #'string-equal))
                                  unless type
                                    do (error "Unable to find type for field ~S."
                                              name)
                                  collect (list :param name type))))
    `(define-rpc-method (,api ,method-name) (,@positional-args &key ,@fields-kwargs)
       ,@param-definitions
       (:result ,model)
       (with-session (user-id)
         (with-connection ()
           (let ((object (progn ,@body)))
             (assert-can-modify user-id object)
             ,@update-code
             (save-dao object)
             (values object)))))))


(defun cached-url-as (url path)
  "Скачивает URL, если получится, кеширует результат по указанному пути и возвращает этот путь.

   Это нужно, чтобы у нас все спеки микросервисов лежали в репозитории и были доступны
   в момент, когда сами сервисы не доступны, а только происходит сборка."
  (let ((content (ignore-errors
                  (dex:get url))))
    (cond
      (content
       (write-string-into-file content path :if-exists :supersede :if-does-not-exist :create))
      ((not (probe-file path))
       (error "Не удалось скачать спеку с URL ~A и на диске её тоже нет."
              url)))
    path))
