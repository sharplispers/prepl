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

;;;; Debugger for prepl

(in-package :prepl)

;;; FIXME: These declaims violate package locks. Are they needed at
;;; all? Seems not.
#+ignore
(declaim (special
          sb-debug::*debug-command-level*
          sb-debug::*real-stack-top* sb-debug::*stack-top*
          sb-debug::*stack-top-hint* sb-debug::*current-frame*
          sb-debug::*flush-debug-errors*))

(defun debug-loop ()
  (let* ((sb-debug::*debug-command-level* (1+ sb-debug::*debug-command-level*))
         (sb-debug::*real-stack-top* (sb-di:top-frame))
         (sb-debug::*stack-top* (or sb-debug::*stack-top-hint*
                                    sb-debug::*real-stack-top*))
         (sb-debug::*stack-top-hint* nil)
         (sb-debug::*current-frame* sb-debug::*stack-top*)
         (continuable (continuable-break-p)))
    (handler-bind ((sb-di:debug-condition
                    (lambda (condition)
                      (princ condition sb-debug::*debug-io*)
                      (sb-int:/show0 "handling d-c by THROWing DEBUG-LOOP-CATCHER")
                      (throw 'debug-loop-catcher nil))))
      (fresh-line)
      ;;(sb-debug::print-frame-call sb-debug::*current-frame* :verbosity 2)
      (loop ;; only valid to way to exit invoke-debugger is by a restart
       (catch 'debug-loop-catcher
         (handler-bind ((error (lambda (condition)
                                 (when sb-debug::*flush-debug-errors*
                                   (clear-input *debug-io*)
                                   (princ condition)
                                   ;; FIXME: Doing input on *DEBUG-IO*
                                   ;; and output on T seems broken.
                                   (format t
                                           "~&error flushed (because ~
                                             ~S is set)"
                                          'sb-debug::*flush-debug-errors*)
                                   (sb-int:/show0 "throwing DEBUG-LOOP-CATCHER")
                                   (throw 'debug-loop-catcher nil)))))

           (if (zerop *break-level*) ; restart added by SBCL
               (repl :continuable continuable)
               (let ((level *break-level*))
                 (with-simple-restart
                     (abort "~@<Reduce debugger level (to break level ~W).~@:>"
                            level)
                   (let ((sb-debug::*debug-restarts* (compute-restarts)))
                     (repl :continuable continuable)))))))
       (throw 'repl-catcher (values :debug :exit))
       ))))


(defun continuable-break-p ()
  (when (eq 'continue
            (restart-name (car (compute-restarts))))
    t))

#+ignore
(when (boundp 'sb-debug::*debug-loop-fun*)
  (setq sb-debug::*debug-loop-fun* #'debug-loop))

(defun print-restarts ()
  ;;  (format *output* "~&Restart actions (select using :continue)~%")
  (format *standard-output* "~&Restart actions (select using :continue)~%")
  (let ((restarts (compute-restarts)))
    (dotimes (i (length restarts))
      (format *standard-output* "~&~2D: ~A~%" i (nth i restarts)))))


#+ignore
(defun debugger (condition)
  "Enter the debugger."
  (let ((old-hook *debugger-hook*))
    (when old-hook
      (let ((*debugger-hook* nil))
        (funcall old-hook condition old-hook))))
  (%debugger condition))

#+ignore
(when (boundp 'sb-debug::*invoke-debugger-fun*)
  (setq sb-debug::*invoke-debugger-fun* #'debugger))

#+ignore
(defun print-condition (condition)
  (format *output* "~&Error: ~A~%" condition))

#+ignore
(defun print-condition-type (condition)
  (format *output* "~&  [Condition type: ~A]~%" (type-of condition)))

#+ignore
(defun %debugger (condition)
  (print-condition condition)
  (print-condition-type condition)
  (princ #\newline *output*)
  (print-restarts)
  (acldebug-loop))


#+ignore
(defun acldebug-loop ()
  (let ((continuable (continuable-break-p)))
    (if continuable
        (aclrepl :continuable t)
        (let ((level *break-level*))
          (with-simple-restart
              (abort "~@<Reduce debugger level (to debug level ~W).~@:>" level)
            (loop
             (repl)))))))

