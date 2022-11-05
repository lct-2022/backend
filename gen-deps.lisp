(in-package :cl-user)

(defun gen-deps ()
  "Функция для перегенерации all-deps.asd"
  (ql:quickload :gen-deps-system)
  (uiop:symbol-call :gen-deps-system :generate
                    :all :except (list :app :chat :platform :passport :rating :common)))
