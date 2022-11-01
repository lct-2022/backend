(uiop:define-package #:app/forms
  (:use #:cl)
  (:import-from #:reblocks-ui/form)
  (:import-from #:app/vars
                #:*inside-form*)
  (:export
   #:with-html-form))
(in-package #:app/forms)


(defmacro with-html-form ((&rest args) &body body )
  `(let ((*inside-form* t))
     (reblocks-ui/form:with-html-form (,@args)
       ,@body)))
