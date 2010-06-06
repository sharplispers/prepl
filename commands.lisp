;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

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

(cl:in-package :prepl)

(defstruct user-command
  (input nil) ; input, maybe a string or form
  (func nil)  ; cmd func entered, overloaded
              ; (:eof :null-command :cmd-error :cmd-ambiguous :history-error)
  (args nil)  ; args for cmd func
  (hnum nil)) ; history number


;;; cmd table entry
(defstruct command-table-entry
  (name nil) ; name of command
  (func nil) ; function handler
  (desc nil) ; short description
  (parsing nil) ; (:string :case-sensitive nil)
  (group nil) ; command group (:cmd or :alias)
  (abbr-len 0)) ; abbreviation length

(defparameter *default-prompt*
  "~:[~3*~;[~:*~D~:[~;~:*:~D~]~:[~;i~]~:[~;c~]] ~]~A~*> "
  "The default prompt.")
(defparameter *prompt* *default-prompt*
  "The current prompt string or formatter function.")
(defparameter *use-short-package-name* t
  "when T, use the shortnest package nickname in a prompt")
(defparameter *dir-stack* nil
  "The top-level directory stack")
(defparameter *command-chars* ":,"
  "Prefix characters for a top-level command")
(defvar *max-history* 100
  "Maximum number of history commands to remember")
(defvar *exit-on-eof* t
  "If T, then exit when the EOF character is entered.")
(defparameter *history* nil
  "History list")
(defparameter *cmd-number* 1
  "Number of the next command")

(declaim (type list *history*))

(defvar *eof-marker* :eof)
(defvar *eof-command* (make-user-command :func :eof))
(defvar *null-command* (make-user-command :func :null-command))

(defmacro define-repl-command
    (name-and-options (&rest args) &body docstring-and-forms)
  (destructuring-bind (name &key parsing abbr-len aliases)
      (if (listp name-and-options) name-and-options (list name-and-options))
    (let ((docstring (when (stringp (car docstring-and-forms))
		       (car docstring-and-forms)))
	  (cmd-name (intern (format nil "~A-CMD" (symbol-name name))
			    :prepl))
	  (name (string-downcase name)))
      `(progn
	 (defun ,cmd-name (,@args)
	   ,@docstring-and-forms)
	 ,@(iter (for alias in (cons name aliases))
		 (collect `(add-command-table-entry
			    ',alias
			    ',abbr-len
			    ',cmd-name
			    ',(if (eq alias name)
				  docstring
				  (format nil "Alias for ~A" name))
			    ',parsing)))))))

(defparameter *cmd-table-hash*
  (make-hash-table :test #'equal))

(defun prompt-package-name ()
  (if *use-short-package-name*
      (car (sort (append
                  (package-nicknames cl:*package*)
                  (list (package-name cl:*package*)))
                 (lambda (a b) (< (length a) (length b)))))
      (package-name cl:*package*)))

(defun read-command (input-stream)
  ;; Reads a command from the user and returns a user-command object
  (let* ((next-char (peek-char-non-whitespace input-stream))
         (cmd (cond
                ((command-char-p next-char)
                 (dispatch-command-line input-stream))
                ((eql #\newline next-char)
                 (read-char input-stream)
                 *null-command*)
                ((eql :eof next-char)
                 *eof-command*)
                (t
                 (let* ((eof (cons nil *eof-marker*))
                        (form (read input-stream nil eof)))
                   (if (eq form eof)
                       *eof-command*
                       (make-user-command :input form :func nil :hnum *cmd-number*)))))))
    (skip-remaining-whitespace input-stream)
    (if (and (eq cmd *eof-command*) (typep input-stream 'string-stream))
        (throw 'repl-catcher cmd)
        cmd)))

(defun command-char-p (char)
  (position char *command-chars*))

(defun dispatch-command-line (input-stream)
  "Processes an input line that starts with *command-chars*"
  (let* ((line (string-trim-whitespace (read-line input-stream)))
         (first-space-pos (position #\space line))
         (cmd-string (subseq line 1 first-space-pos))
         (cmd-args-string
          (if first-space-pos
              (string-trim-whitespace (subseq line first-space-pos))
              "")))
    (declare (simple-string line))
    (cond
      ((or (zerop (length cmd-string))
           (whitespace-char-p (char cmd-string 0)))
       *null-command*)
      ((or (numberp (read-from-string cmd-string))
           (char= (char cmd-string 0) #\+)
           (char= (char cmd-string 0) #\-))
       (process-command/numeric cmd-string cmd-args-string))
      ((command-char-p (char cmd-string 0))
       (process-history-search (subseq cmd-string 1) cmd-args-string))
      (t
       (multiple-value-bind
	   (override user-command full-name)
	   (process-command/text cmd-string line cmd-args-string)
	 (or (unless (or (eq override :override-not-allowed)
			 (zerop (command-char-p (elt line 0))))
	       (process-command/override (or full-name cmd-string) 
					 line 
					 cmd-args-string 
					 override
					 user-command))
	     user-command))))))

(defun process-command/numeric (cmd-string cmd-args-string)
  "Process a numeric cmd, such as ':123'"
  (let* ((first-char (char cmd-string 0))
         (number-string (if (digit-char-p first-char)
                            cmd-string
                            (subseq cmd-string 1)))
         (is-minus (char= first-char #\-))
         (raw-number (read-from-string number-string))
         (number (if is-minus
                     (- *cmd-number* raw-number)
                     raw-number))
         (cmd (get-history number)))
    (when (eq cmd *null-command*)
      (return-from process-command/numeric
        (make-user-command :func :history-error :input (read-from-string
                                                    cmd-string))))
    (maybe-return-history-command cmd cmd-args-string)))

(defun maybe-return-history-command (cmd cmd-args-string)
  (format *output* "~A~%" (user-command-input cmd))
  (let ((dont-redo
         (when (and (stringp cmd-args-string)
                    (plusp (length cmd-args-string))
                    (char= #\? (char cmd-args-string 0)))
           (do ((line nil (read-line *input*)))
               ((and line (or (zerop (length line))
                              (string-equal line "Y")
                              (string-equal line "N")))
                (when (string-equal line "N")
                  t))
             (when line
               (format *output* "Type \"y\" for yes or \"n\" for no.~%"))
             (format *output* "redo? [y] ")
             (force-output *output*)))))
    (if dont-redo
        *null-command*
        (make-user-command :func (user-command-func cmd)
                       :input (user-command-input cmd)
                       :args (user-command-args cmd)
                       :hnum *cmd-number*))))


(defun find-history-matching-pattern (cmd-string)
  "Return history item matching cmd-string or NIL if not found"
  (dolist (his *history* nil)
    (let* ((input (user-command-input his))
           (string-input (if (stringp input)
                             input
                             (write-to-string input))))
      (when (search cmd-string string-input :test #'string-equal)
        (return-from find-history-matching-pattern his)))))

(defun process-history-search (pattern cmd-args-string)
  (let ((cmd (find-history-matching-pattern pattern)))
    (unless cmd
      (format *output* "No match on history list with pattern ~S~%" pattern)
      (return-from process-history-search *null-command*))
    (maybe-return-history-command cmd cmd-args-string)))

(defun parse-args (parsing args-string)
  (case parsing
    (:string
     (if (zerop (length args-string))
	 nil
	 (list args-string)))
    (t
     (let ((string-stream (make-string-input-stream args-string))
	   (eof (cons nil *eof-marker*))) ;new cons for eq uniqueness
       (loop as arg = (read string-stream nil eof)
	 until (eq arg eof)
	 collect arg)))))

(defun process-command/text (cmd-string line cmd-args-string)
  "Process a text cmd, such as ':ld a b c'"
  (multiple-value-bind (cmd-entry all-matches)
      (completing-find-command cmd-string)
    (unless cmd-entry
      (return-from process-command/text
        (if all-matches
	    (values
	     :do-not-override
	     (make-user-command :func :cmd-ambiguous :input all-matches))
	    (values
	     nil
	     (make-user-command :func :cmd-error :input cmd-string)))))
    (let ((parsing (command-table-entry-parsing cmd-entry)))
      (values (or parsing t)
	      (make-user-command :func (command-table-entry-func cmd-entry)
				 :input line
				 :args (parse-args parsing cmd-args-string)
				 :hnum *cmd-number*)
	      (command-table-entry-name cmd-entry)))))

(defvar *next-command*)

(defun call-next-command (&rest args)
  (apply (or (user-command-func *next-command*)
	     (error "no next command"))
	 args ))

(defun process-command/override
       (cmd line cmd-args-string override original-command)
  (dolist (hook *command-parser-hooks*)
    (multiple-value-bind (fun parsing)
        (funcall hook cmd override)
      (when fun
	(return
	 (make-user-command :func (lambda (&rest args)
				    (let ((*next-command* original-command))
				      (apply fun args)))
			    :input line
			    :args (parse-args parsing cmd-args-string)
			    :hnum *cmd-number*))))))

(defun make-cte (name-param func desc parsing group abbr-len)
  (let ((name (etypecase name-param
                (string
                 name-param)
                (symbol
                 (string-downcase (write-to-string name-param))))))
    (make-command-table-entry :name name :func func :desc desc
                          :parsing parsing :group group
                          :abbr-len (if abbr-len
                                        abbr-len
                                        (length name)))))

(defun %add-entry (cmd &optional abbr-len)
  (let* ((name (command-table-entry-name cmd))
         (alen (if abbr-len
                   abbr-len
                   (length name))))
    (dotimes (i (length name))
      (when (>= i (1- alen))
        (setf (gethash (subseq name 0 (1+ i)) *cmd-table-hash*)
              cmd)))))

(defun add-command-table-entry (cmd-string abbr-len func-name desc parsing)
  (%add-entry
   (make-cte cmd-string (symbol-function func-name) desc parsing :cmd abbr-len)
   abbr-len))

(defun find-command (cmdstr)
  (gethash (string-downcase cmdstr) *cmd-table-hash*))

(defun completing-find-command (cmdstr)
  (or (find-command cmdstr)
      (let ((matches
	     (iter (for (name cmd) in-hashtable *cmd-table-hash*)
		   (let ((mismatch (mismatch name cmdstr)))
		     (when (eq mismatch (length cmdstr))
		       (collect cmd))))))
	(cond
	  ((null matches)
	   nil)
	  ((cddr matches)
	   (values nil (mapcar #'command-table-entry-name matches)))
	  (t
	   (car matches))))))

(defun user-command= (c1 c2)
  "Returns T if two user commands are equal"
  (and (eq (user-command-func c1) (user-command-func c2))
       (equal (user-command-args c1) (user-command-args c2))
       (equal (user-command-input c1) (user-command-input c2))))

(defun add-to-history (cmd)
  (unless (and *history* (user-command= cmd (car *history*)))
    (when (>= (length *history*) *max-history*)
      (setq *history* (nbutlast *history*
                                (1+ (- (length *history*) *max-history*)))))
    (push cmd *history*)
    (incf *cmd-number*)))

(defun get-history (n)
  (let ((cmd (find n *history* :key #'user-command-hnum :test #'eql)))
    (if cmd
        cmd
        *null-command*)))

(defun get-command-doc-list (&optional (group :cmd))
  "Return list of all commands"
  (let ((cmds '()))
    (maphash (lambda (k v)
               (when (and
                      (= (length k) (length (command-table-entry-name v)))
                      (eq (command-table-entry-group v) group))
                 (push (list k
                             (if (= (command-table-entry-abbr-len v)
                                    (length k))
                                  ""
                                  (subseq k 0 (command-table-entry-abbr-len v)))
                             (command-table-entry-desc v)) cmds)))
             *cmd-table-hash*)
    (sort cmds #'string-lessp :key #'car)))

(define-repl-command (cd :parsing :string) (&optional string-dir)
  "change default directory"
  (cond
    ((or (zerop (length string-dir))
         (string= string-dir "~"))
     (setf cl:*default-pathname-defaults* (user-homedir-pathname)))
    (t
     (let ((new (truename string-dir)))
       (when (pathnamep new)
         (setf cl:*default-pathname-defaults* new)))))
  (format *output* "~A~%" (namestring cl:*default-pathname-defaults*))
  (values))

(define-repl-command pwd ()
  "print current directory"
  (format *output* "Lisp's current working directory is ~s.~%"
          (namestring cl:*default-pathname-defaults*))
  (values))

(define-repl-command trace (&rest args)
  "trace a function"
  (format *output* "~A~%" (eval `(trace ,@args)))
  (values))

(define-repl-command untrace (&rest args)
  "untrace a function"
  (format *output* "~A~%" (eval `(untrace ,@args)))
  (values))

(defun other-threads ()
  "Returns a list of all threads except the current one"
  (remove (bordeaux-threads:current-thread) (bordeaux-threads:all-threads)))

(defun quit (status)
  #+sbcl (sb-ext:quit :unix-status status)
  #+openmcl (ccl:quit status)
  #-(or sbcl openmcl) (error "Sorry, don't know how to quit on this Lisp."))

(define-repl-command exit (&optional (status 0))
  "exit lisp"
  (let ((other-threads (other-threads)))
    (when other-threads
      (format *output* "There exists the following processes~%")
      (format *output* "~{~A~%~}" other-threads)
      (format *output* "Do you want to exit lisp anyway [n]? ")
      (force-output *output*)
      (let ((input (string-trim-whitespace (read-line *input*))))
        (if (and (plusp (length input))
                 (or (char= #\y (char input 0))
                     (char= #\Y (char input 0))))
            ;; loop in case more threads get created while trying to exit
            (do ((threads other-threads (other-threads)))
                ((eq nil threads))
              (map nil #'bordeaux-threads:destroy-thread threads)
              (sleep 0.2))
            (return-from exit-cmd)))))
  (quit status)
  (values))

(define-repl-command package (&optional pkg)
  "change current package"
  (cond
    ((null pkg)
     (format *output* "The ~A package is current.~%"
             (package-name cl:*package*)))
    ((null (find-package (write-to-string pkg)))
     (format *output* "Unknown package: ~A.~%" pkg))
    (t
     (setf cl:*package* (find-package (write-to-string pkg)))))
  (values))

(defun readtable-name-for-repl (table)
  ;; don't want :CURRENT as a readtable name
  (let ((name (named-readtables:readtable-name table)))
    (if (and name (not (eq name :current)))
	name
	*readtable*)))

(define-repl-command readtable (&optional name)
  "change current readtable"
  (cond
    (name
     (let ((table (named-readtables:find-readtable name)))
       (if table
	   (prog1
	       (setf *readtable* (named-readtables:find-readtable name))
	     (format *output* "The ~A readtable is now current.~%"
		     (readtable-name-for-repl *readtable*)))
	   (format *output* "Unknown readtable: ~A.~%" name))))
    (t
     (format *output* "The ~A readtable is current.~%"
	     (readtable-name-for-repl *readtable*))
     *readtable*)))

(defun string-to-list-skip-spaces (str)
  "Return a list of strings, delimited by spaces, skipping spaces."
  (declare (type (or null string) str))
  (when str
    (loop for i = 0 then (1+ j)
          as j = (position #\space str :start i)
          when (not (char= (char str i) #\space))
          collect (subseq str i j) while j)))

(let ((last-files-loaded nil))
  (define-repl-command (ld :parsing :string) (&optional string-files)
    "load a file"
    (if string-files
        (setq last-files-loaded string-files)
        (setq string-files last-files-loaded))
    (dolist (arg (string-to-list-skip-spaces string-files))
      (let ((file
             (if (string= arg "~/" :end1 1 :end2 1)
                 (merge-pathnames (parse-namestring
                                   (string-left-trim "~/" arg))
                                  (user-homedir-pathname))
                 arg)))
        (format *output* "loading ~S~%" file)
        (load file))))
  (values))

(define-repl-command (cf :parsing :string) (string-files)
  "compile file"
  (when string-files
    (dolist (arg (string-to-list-skip-spaces string-files))
      (compile-file arg)))
  (values))

(defun >-num (x y)
  "Return if x and y are numbers, and x > y"
  (and (numberp x) (numberp y) (> x y)))

(defun newer-file-p (file1 file2)
  "Is file1 newer (written later than) file2?"
  (>-num (if (probe-file file1) (file-write-date file1))
         (if (probe-file file2) (file-write-date file2))))

(defun compile-file-as-needed (src-path)
  "Compiles a file if needed, returns path."
  (let ((dest-path (compile-file-pathname src-path)))
    (when (or (not (probe-file dest-path))
              (newer-file-p src-path dest-path))
      (ensure-directories-exist dest-path)
      (compile-file src-path :output-file dest-path))
    dest-path))

;;;; implementation of commands

(define-repl-command (apropos :parsing :string) (string)
  "show apropos"
  (apropos (string-upcase string))
  (fresh-line *output*)
  (values))

(let ((last-files-loaded nil))
  (define-repl-command (cload :parsing :string) (&optional string-files)
    "compile if needed and load file"
    (if string-files
        (setq last-files-loaded string-files)
        (setq string-files last-files-loaded))
    (dolist (arg (string-to-list-skip-spaces string-files))
      (format *output* "loading ~a~%" arg)
      (load (compile-file-as-needed arg)))
    (values)))

(define-repl-command inspect (arg)
  "inspect an object"
  (inspector-fun (eval arg) nil *output*)
  (values))

(define-repl-command istep (&optional arg-string)
  "navigate within inspection of a lisp object"
  (istep (string-to-list-skip-spaces arg-string) *output*)
  (values))

(define-repl-command describe (&rest args)
  "describe an object"
  (dolist (arg args)
    (eval `(describe ,arg)))
  (values))

(define-repl-command macroexpand (arg)
  "macroexpand an expression"
  (pprint (macroexpand arg) *output*)
  (values))

(define-repl-command history ()
  "print the recent history"
  (let ((n (length *history*)))
    (declare (fixnum n))
    (dotimes (i n)
      (declare (fixnum i))
      (let ((hist (nth (- n i 1) *history*)))
        (format *output* "~3A " (user-command-hnum hist))
        (if (stringp (user-command-input hist))
            (format *output* "~A~%" (user-command-input hist))
            (format *output* "~W~%" (user-command-input hist))))))
  (values))

(define-repl-command help (&optional cmd)
  "print this help"
  (cond
    (cmd
     (let ((cmd-entry (completing-find-command cmd)))
       (if cmd-entry
           (format *output* "Documentation for ~A: ~A~%"
                   (command-table-entry-name cmd-entry)
                   (command-table-entry-desc cmd-entry)))))
    (t
     (format *output* "Command characters are ~{'~A'~^ and ~}.~%"
	     (coerce *command-chars* 'list))
     (format *output* "Names can be abbreviated to any unique prefix.~%")
     (format *output* "~%Full list of commands:~%~%")
     (format *output* "~11A ~4A ~A~%" "COMMAND" "" "DESCRIPTION")
     (format *output* "~11A ~4A ~A~%" "<n>" ""
             "re-execute <n>th history command")
     (dolist (doc-entry (get-command-doc-list :cmd))
       (format *output* "~11A ~4A ~A~%" (first doc-entry)
               (second doc-entry) (third doc-entry)))))
  (values))

(define-repl-command aliases ()
  "show aliases"
  (let ((doc-entries (get-command-doc-list :alias)))
    (typecase doc-entries
      (cons
       (format *output* "~11A ~A ~4A~%" "ALIAS" "ABBR" "DESCRIPTION")
       (dolist (doc-entry doc-entries)
         (format *output* "~11A ~4A ~A~%" (first doc-entry) (second doc-entry) (third doc-entry))))
      (t
       (format *output* "No aliases are defined~%"))))
  (values))

#+nil
;; later, this command can be defined portably in hemlock, which has a
;; suitable process abstraction.
(define-repl-command shell (string-arg)
  (sb-ext:run-program "/bin/sh" (list "-c" string-arg)
                      :input nil :output *output*)
  (values))

(define-repl-command (pushd :parsing :string) (string-arg)
  "push directory on stack"
  (push string-arg *dir-stack*)
  (cd-cmd string-arg)
  (values))

(define-repl-command popd ()
  "pop directory from stack"
  (if *dir-stack*
      (let ((dir (pop *dir-stack*)))
        (cd-cmd dir))
      (format *output* "No directory on stack to pop.~%"))
  (values))

(define-repl-command pop (&optional (n 1))
  "pop up `n' (default 1) break levels"
  (cond
    (*inspect-break*
     (throw 'repl-catcher (values :inspect n)))
    ((plusp *break-level*)
     (throw 'repl-catcher (values :pop n))))
  (values))

(defvar *current-error* nil)
(defvar *debugging-context* nil)

(define-repl-command bt (&optional (n most-positive-fixnum))
  "backtrace `n' stack frames, default all"
  (conium:call-with-debugging-environment
   (lambda ()
     (mapcar (lambda (frame)
	       (conium:print-frame frame *standard-output*)
	       (fresh-line))
	     (conium:compute-backtrace 0 n)))))

(define-repl-command current ()
  "print the expression for the current stack frame"
  (if *current-error*
      (describe *current-error*)
      (write-line "No error.")))

(define-repl-command top ()
  "move to top stack frame"
  #+implement-the-debugger (sb-debug::frame-debug-command 0))

(define-repl-command bottom ()
  "move to bottom stack frame"
  #+implement-the-debugger (sb-debug::bottom-debug-command))

(define-repl-command up (&optional (n 1))
  (declare (ignore n))
  "move up `n' stack frames, default 1"
  #+implement-the-debugger
  (dotimes (i n)
    (if (and sb-debug::*current-frame*
             (sb-di:frame-up sb-debug::*current-frame*))
        (sb-debug::up-debug-command)
        (progn
          (format *output* "Top of the stack")
          (return-from up-cmd)))))

(define-repl-command dn (&optional (n 1))
  (declare (ignore n))
  "move down `n' stack frames, default 1"
  #+implement-the-debugger
  (dotimes (i n)
    (if (and sb-debug::*current-frame*
             (sb-di:frame-down sb-debug::*current-frame*))
        (sb-debug::down-debug-command)
        (progn
          (format *output* "Bottom of the stack")
          (return-from dn-cmd)))))

(define-repl-command continue (&optional (num 0))
  "continue from a continuable error"
  ;; don't look at first restart
  (let ((restarts (compute-restarts)))
    (if restarts
        (let ((restart
               (typecase num
                 (unsigned-byte
                  (if (< -1 num (length restarts))
                      (nth num restarts)
                      (progn
                        (format *output* "There is no such restart")
                        (return-from continue-cmd))))
                 (symbol
                  (find num (the list restarts)
                        :key #'restart-name
                        :test (lambda (sym1 sym2)
                                (string= (symbol-name sym1)
                                         (symbol-name sym2)))))
                 (t
                  (format *output* "~S is invalid as a restart name" num)
                  (return-from continue-cmd nil)))))
          (when restart
            (invoke-restart-interactively restart)))
    (format *output* "~&There are no restarts"))))

(define-repl-command abort ()
  "Invoke ABORT restart."
  ;; don't look at first restart
  (when (find-restart 'abort)
    (invoke-restart 'abort))
  (format *output* "~&No abort restart found."))

(define-repl-command error ()
  "print the last error message"
  (if *current-error*
      (format t "~&Current condition: ~S:~%  ~:*~A~%" *current-error*)
      (format t "~&No current error.~%"))
  (terpri)
  (show-restarts)
  (shiftf *** ** * *current-error*))

(defun show-restarts ()
  (format t "Available restarts:~%")
  (let ((shadowing-names '()))
    (iter (for restart in (compute-restarts))
	  (for i from 0)
	  (let ((name (restart-name restart)))
	    (if (find name shadowing-names)
		(setf name nil)
		(push name shadowing-names))
	    (format t "~4D ~@[[~A]~]~30T~A~%" i name restart)))))

(define-repl-command frame ()
  "print info about the current frame"
  #+implement-the-debugger
  (sb-debug::print-frame-call sb-debug::*current-frame*))

(define-repl-command zoom ()
  "print the runtime stack"
  (conium:call-with-debugging-environment
   (lambda ()
     (mapcar (lambda (frame)
	       (conium:print-frame frame *standard-output*)
	       (fresh-line))
	     (conium:compute-backtrace 0 most-positive-fixnum)))))

(define-repl-command local (&optional var)
  "print the value of a local variable"
  (declare (ignore var))
  #+implement-the-debugger
  (sb-debug::list-locals-debug-command))

(define-repl-command processes ()
  (dolist (thread (bordeaux-threads:all-threads))
    (format *output* "~&~A~20T~:[dead~;alive~]~@[~20T[current thread]~]"
	    (bordeaux-threads:thread-name thread)
	    (bordeaux-threads:thread-alive-p thread)
	    (eq thread (bordeaux-threads:current-thread))))
  (values))

(define-repl-command kill (&rest selected-threads)
  "kill (destroy) processes"
  (dolist (thread selected-threads)
    (let ((found (find thread (bordeaux-threads:all-threads)
		       :key 'bordeaux-threads:thread-name
                       :test 'equal)))
      (if found
          (progn
            (format *output* "~&Destroying thread ~A" thread)
            (bordeaux-threads:destroy-thread found))
          (format *output* "~&Thread ~A not found" thread))))
  (values))

#+nil
;; Cute idea, but a hassle to get right and somewhat useless in a
;; multi-buffer GUI like hemlock.
(define-repl-command focus (&optional process)
  (declare (ignore process)))

(define-repl-command reset ()
  "reset to top break level"
  (when (find-restart 'abort-to-outmost-repl)
    (invoke-restart 'abort-to-outmost-repl)))

(define-repl-command dirs ()
  "show directory stack"
  (dolist (dir *dir-stack*)
    (format *output* "~a~%" dir))
  (values))

(define-repl-command (load-op :aliases ("make" "load-system")
			      :parsing :string) 
		     (name)
  "Load the specified ASDF system"
  (asdf:operate 'asdf:load-op name)
  (prin1 (asdf:find-system name)))


;;;; machinery for aliases

;;;; Fixme:
;;;;
;;;;  - ALIAS is a bad name, dating back to Allegro.
;;;;    We should only be talking about commands.
;;;;
;;;;  - I don't see a reason to have a distiction between built-in commands
;;;;    and user-defined commands.
;;;;
;;;;  - Little differences between DEFINE-REPL-COMMAND and ALIAS remain.
;;;;
;;;; Need to get rid of ALIAS in favour of DEFINE-REPL-COMMAND at some point.

(defsetf alias (name &key abbr-len description) (user-func)
  `(progn
    (%add-entry
     (make-cte (quote ,name) ,user-func ,description nil :alias ,abbr-len))
    (quote ,name)))

(defmacro alias (name-param args &rest body)
  (let ((parsing nil)
        (desc "")
        (abbr-index nil)
        (name (if (atom name-param)
                  name-param
                  (car name-param))))
    (when (consp name-param)
     (dolist (param (cdr name-param))
        (cond
          ((or
            (eq param :case-sensitive)
            (eq param :string))
           (setq parsing param))
          ((stringp param)
           (setq desc param))
          ((numberp param)
           (setq abbr-index param)))))
    `(progn
      (%add-entry
       (make-cte (quote ,name) (lambda ,args ,@body) ,desc ,parsing :alias (when ,abbr-index
                                                                               (1+ ,abbr-index)))
       ,abbr-index)
      ,name)))


(defun remove-alias (&rest aliases)
  (declare (list aliases))
  (let ((keys '())
        (remove-all (not (null (find :all aliases)))))
    (unless remove-all  ;; ensure all alias are strings
      (setq aliases
            (loop for alias in aliases
                  collect
                  (etypecase alias
                    (string
                     alias)
                    (symbol
                     (symbol-name alias))))))
    (maphash
     (lambda (key cmd)
       (when (eq (command-table-entry-group cmd) :alias)
         (if remove-all
             (push key keys)
             (when (some
                    (lambda (alias)
                      (let ((klen (length key)))
                        (and (>= (length alias) klen)
                             (string-equal (subseq alias 0 klen)
                                           (subseq key 0 klen)))))
                    aliases)
               (push key keys)))))
     *cmd-table-hash*)
    (dolist (key keys)
      (remhash key *cmd-table-hash*))
    keys))

;;;; low-level reading/parsing functions

;;; Skip white space (but not #\NEWLINE), and peek at the next
;;; character.
(defun peek-char-non-whitespace (&optional stream)
  (do ((char (peek-char nil stream nil *eof-marker*)
             (peek-char nil stream nil *eof-marker*)))
      ((not (whitespace-char-not-newline-p char)) char)
    (read-char stream)))

(defun string-trim-whitespace (str)
  (string-trim '(#\space #\tab #\return)
               str))

(defun whitespace-char-p (x)
  (and (characterp x)
       (or (char= x #\space)
           (char= x #\tab)
           (char= x #\page)
           (char= x #\newline)
           (char= x #\return))))

(defun whitespace-char-not-newline-p (x)
  (and (whitespace-char-p x)
       (not (char= x #\newline))))

(defun skip-remaining-whitespace (&optional stream)
  (iter
   (let ((char (read-char-no-hang stream nil *eof-marker*)))
     (while char)
     (until (eq char *eof-marker*))
     (unless (whitespace-char-p char)
       (unread-char char stream)
       (return)))))

;;;; the following functions used to be hooks in SBCL

(defun frame-number ()
  #+implement-the-debugger (when (and (plusp *break-level*)
				      sb-debug::*current-frame*)
			     (sb-di::frame-number sb-debug::*current-frame*))
  nil)

(defvar *prompt-hooks*
  (list #+sbcl #'sb-thread::get-foreground))

(defun prompt (stream)
  (let ((break-level (when (plusp *break-level*)
                       *break-level*))
        (frame-number (frame-number)))
    (run-hooks *prompt-hooks*)
    (fresh-line stream)
    (if (functionp *prompt*)
        (write-string (funcall *prompt*
                               break-level
                               frame-number
                               *inspect-break*
                               *continuable-break*
                               (prompt-package-name)
			       *cmd-number*)
                      stream)
        (handler-case
            (format nil *prompt*
                    break-level
                    frame-number
                    *inspect-break*
                    *continuable-break*
                    (prompt-package-name) *cmd-number*)
          (error ()
            (format stream "~&Prompt error>  "))
          (:no-error (prompt)
            (format stream "~A" prompt))))))

(defun process-command (user-command)
  "list all processes"
  ;; Processes a user command. Returns t if the user-command was a top-level
  ;; command
  (cond ((eq user-command *eof-command*)
         (when *exit-on-eof*
           (quit 0))
         (format *output* "EOF~%")
         t)
        ((eq user-command *null-command*)
         t)
        ((eq (user-command-func user-command) :cmd-error)
         (format *output* "Unknown top-level command: ~s.~%"
                 (user-command-input user-command))
         (format *output* "Type `~Ahelp' for the list of commands.~%" (elt *command-chars* 0))
         t)
	((eq (user-command-func user-command) :cmd-ambiguous)
         (format *output* "Ambiguous top-level command. Completions are:~{~%  ~A~}.~%"
                 (user-command-input user-command))
         t)
        ((eq (user-command-func user-command) :history-error)
         (format *output* "Input numbered ~d is not on the history list~%"
                 (user-command-input user-command))
         t)
        ((functionp (user-command-func user-command))
         (add-to-history user-command)
         (apply (user-command-func user-command) (user-command-args user-command))
         t)
        (t
         (add-to-history user-command)
         nil))) ; nope, not in my job description

(defmacro rebinding ((&rest vars) &body body)
  `(let (,@(mapcar (lambda (var) `(,var ,var)) vars))
     ,@body))

#+sbcl
;; unwinding through with-new-session seems to wreak havoc, so let's only
;; wrap this around the outermost repl.
(defvar *in-session-workaround* nil)

(defun invoke-with-session-workaround-if-on-sbcl (fun)
  #+sbcl (if *in-session-workaround*
	     (funcall fun)
	     (let ((*in-session-workaround* t))
	       (sb-thread:with-new-session () (funcall fun))))
  #-sbcl (funcall fun))

(defmacro session-workaround-if-on-sbcl (&rest forms)
  `(invoke-with-session-workaround-if-on-sbcl (lambda () ,@forms)))

(defvar *entering-prepl-debugger-hook* nil)

(defun debugger (condition hook &optional pre-repl-fun)
  (declare (ignore hook))
  (let ((*current-error* condition)
	(*debugging-context* (gensym)))
    (flet ((cont ()
	     (format t "~&Debugger entered for condition: ~S:~%  ~:*~A~%"
		     *current-error*)
	     (show-restarts)
	     (conium:call-with-debugging-environment
	      (lambda ()
		(when pre-repl-fun (funcall pre-repl-fun))
		(repl)))))
      (if *entering-prepl-debugger-hook*
	  (funcall *entering-prepl-debugger-hook* #'cont)
	  (cont)))))

(defun repl (&rest args &key break-level noprint inspect continuable nobanner)
  (declare (ignore break-level noprint inspect continuable nobanner))
  (rebinding
      (*break-level* *inspect-break* *continuable-break*
		     *dir-stack* *command-chars* *prompt*
		     *use-short-package-name* *max-history* *exit-on-eof*
		     *history* *cmd-number*)
      (conium:call-with-debugger-hook
       #'debugger
       (lambda ()
	 (session-workaround-if-on-sbcl (apply #'%repl args))))))

(defun global-prepl-debugger-hook (condition hook)
  ;; (session-workaround-if-on-sbcl (lambda ()))
  (debugger condition hook))

(defun install-global-prepl-debugger-hook ()
  (conium:install-debugger-globally #'global-prepl-debugger-hook))

