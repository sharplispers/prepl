;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :prepl
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "prepl"))
  :depends-on (#+nil :cl-ppcre))
