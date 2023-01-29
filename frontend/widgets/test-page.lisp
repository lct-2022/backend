(uiop:define-package #:app/widgets/test-page
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html))
(in-package #:app/widgets/test-page)


(defwidget test-page ()
  ())


(defun make-test-page ()
  (make-instance 'test-page))


(defmethod render ((widget test-page))
  (with-html
    ;; (break)
    (:p "Hello world.")))
