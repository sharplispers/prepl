;;; -*- mode: lisp; indent-tabs: nil -*-

;;; This file was taken from SBCL's sb-aclrepl contrib, written by Keven
;;; Rosenberg and available under SBCL's public domain status.
;;;
;;; Changes since then are:

;;; Copyright (c) 2009 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :prepl)

(defvar *noprint* nil
  "boolean: T if don't print prompt and output")
(defvar *break-level* -1
  "current break level")
(defvar *inspect-break* nil
  "boolean: T if break caused by inspect")
(defvar *continuable-break* nil
  "boolean: T if break caused by continuable error")

(defvar *unwind-hooks*
  (list #+sbcl #'sb-impl::disable-stepping))

(defun run-hooks (hooks &rest args)
  (mapc (lambda (hookfun)
	  (apply hookfun args))
	hooks))

(defvar *input*)
(defvar *output*)

(defvar *outmost-repl* t)

(defun %repl (&key
             (break-level (1+ *break-level*))
             (noprint *noprint*)
             (inspect nil)
             (continuable nil))
  (let ((*noprint* noprint)
        (*break-level* break-level)
        (*inspect-break* inspect)
        (*continuable-break* continuable))
    (iter
     (if *outmost-repl*
	 (with-simple-restart (abort "Abort to REPL")
	   (let ((*outmost-repl* nil))
	     (until (rep-one))))
	 (until (rep-one))))))

(defun interactive-eval (form)
  "Evaluate FORM, returning whatever it returns and adjusting ***, **, *,
+++, ++, +, ///, //, /, and -."
  (setf - form)
  (unwind-protect
       (let ((results (multiple-value-list (eval form))))
         (setf /// //
               // /
               / results
               *** **
               ** *
               * (car results)))
    (setf +++ ++
          ++ +
          + -))
  (unless (boundp '*)
    ;; The bogon returned an unbound marker.
    ;; FIXME: It would be safer to check every one of the values in RESULTS,
    ;; instead of just the first one.
    (setf * nil)
    (cerror "Go on with * set to NIL."
            "EVAL returned an unbound marker."))
  (values-list /))

(defvar *after-prompt-hooks*
  (list #+sbcl #'sb-sys:scrub-control-stack))

(defun rep-one ()
  (multiple-value-bind (reason reason-param)
      (catch 'repl-catcher
	(unwind-protect
	     (%rep-one)
	  (run-hooks *unwind-hooks*)))
    (declare (ignore reason-param))
    (or (and (eq reason :inspect)
	     (plusp *break-level*))
	(and (eq reason :pop)
	     (plusp *break-level*)))))

(defun %rep-one ()
  "Read-Eval-Print one form"
  ;; (See comment preceding the definition of SCRUB-CONTROL-STACK.)
  (run-hooks *after-prompt-hooks*)
  (unless *noprint*
    (prompt *standard-output*)
    (force-output *standard-output*))
  (let* ((*input* *standard-input*)
	 (*output* *standard-output*)
	 (user-command (read-command *input*))
	 (level *break-level*))
    (unless (process-command user-command)
      (with-simple-restart (abort
			    "~@<Reduce debugger level (to debug level ~W).~@:>"
			    level)
	(let ((results
	       (multiple-value-list
		(interactive-eval
		 (user-command-input user-command)))))
	  (unless *noprint*
	    (dolist (result results)
	      (prin1 result *standard-output*)
	      (fresh-line *standard-output*))))))))
