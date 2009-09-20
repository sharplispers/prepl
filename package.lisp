;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :prepl
  (:use :cl :iterate)
  (:export "REPL"

	   "ALIAS"

	   "*PROMPT*"
	   "*EXIT-ON-EOF*"
	   "*MAX-HISTORY*"
	   "*USE-SHORT-PACKAGE-NAME*"
	   "*COMMAND-CHAR*"))
