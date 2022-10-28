(uiop:define-package #:common/rpc
  (:use #:cl)
  (:import-from #:closer-mop
                #:slot-definition-type
                #:slot-definition-name
                #:class-slots
                #:ensure-finalized)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:mito
                #:save-dao))
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
         (fields-kwargs (loop for name in fields
                              for given-name = (symbolicate name "-GIVEN-P")
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
    `(define-rpc-method (,api ,method-name) (&key ,@fields-kwargs)
       ,@param-definitions
       (:result ,model)
       (with-connection ()
         (let ((object (progn ,@body)))
           ,@update-code
           (save-dao object)
           (values object))))))



