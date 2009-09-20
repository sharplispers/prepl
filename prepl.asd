;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :prepl
  :serial t
  :components ((:file "package")
	       (:file "toplevel")
	       (:file "repl")
	       (:file "inspect"))
  :depends-on (:closer-mop :iterate :bordeaux-threads :trivial-backtrace))
