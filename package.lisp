;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :prepl
  (:use :cl :iterate)
  (:export "REPL"

	   "DEFINE-REPL-COMMAND"
	   "ALIAS"

	   "*PROMPT*"
	   "*EXIT-ON-EOF*"
	   "*MAX-HISTORY*"
	   "*USE-SHORT-PACKAGE-NAME*"
	   "*COMMAND-CHAR*"))
