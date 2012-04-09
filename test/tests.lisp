;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:CL-WHO-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-who/test/tests.lisp,v 1.5 2009/01/26 11:10:52 edi Exp $

;;; Copyright (c) 2008-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

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

(in-package :cl-who-test)

(defvar *initial-settings*
  (list #\'
        t
        (lambda (char)
          (or (find char "<>&'\"")
              (> (char-code char) 127)))
        t
        '(:area
          :atop
          :audioscope
          :base
          :basefont
          :br
          :choose
          :col
          :frame
          :hr
          :img
          :input
          :isindex
          :keygen
          :left
          :limittext
          :link
          :meta
          :nextid
          :of
          :over
          :param
          :range
          :right
          :spacer
          :spot
          :tab
          :wbr)
        "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The location of this source file.")

(defmacro do-tests ((name &optional show-progress-p) &body body)
  "Helper macro which repeatedly executes BODY until the code in body
calls the function DONE.  It is assumed that each invocation of BODY
will be the execution of one test which returns NIL in case of success
and list of string describing errors otherwise.

The macro prints a simple progress indicator \(one dots for ten tests)
to *STANDARD-OUTPUT* unless SHOW-PROGRESS-P is NIL and returns a true
value iff all tests succeeded.  Errors in BODY are caught and reported
\(and counted as failures)."
  `(let ((successp t)
         (testcount 1))
     (block test-block
       (flet ((done ()
                (return-from test-block successp)))
         (format t "~&Test: ~A~%" ,name)
         (loop
          (when (and ,show-progress-p (zerop (mod testcount 1)))
            (format t ".")
            (when (zerop (mod testcount 10))
              (terpri))
            (force-output))
          (let ((errors
                 (handler-case
                     (progn ,@body)
                   (error (msg)
                     (list (format nil "~&got an unexpected error: ~A" msg))))))
            (setq successp (and successp (null errors)))
            (when errors
              (format t "~&~4@A:~{~&   ~A~}~%" testcount errors))
            (incf testcount)))))
     successp))

(defun simple-tests (&key (file-name
                           (make-pathname :name "simple"
                                          :type nil :version nil
                                          :defaults *this-file*))
                          (external-format '(:latin-1 :eol-style :lf))
                          verbose)
  "Loops through all the forms in the file FILE-NAME and executes each
of them using EVAL.  It is assumed that each FORM specifies a test
which returns a true value iff it succeeds.  Prints each test form to
*STANDARD-OUTPUT* if VERBOSE is true and shows a simple progress
indicator otherwise.  EXTERNAL-FORMAT is the FLEXI-STREAMS external
format which is used to read the file.  Returns a true value iff all
tests succeeded."
  (with-open-file (binary-stream file-name :element-type 'flex:octet)
    (let ((stream (flex:make-flexi-stream binary-stream :external-format external-format))
          (*package* (find-package :cl-who-test))
          (html-mode (html-mode)))
      (unwind-protect
          (destructuring-bind (*attribute-quote-char*
                               *downcase-tokens-p*
                               *escape-char-p*
                               *html-empty-tag-aware-p*
                               *html-empty-tags*
                               *prologue*)
              *initial-settings*
            (setf (html-mode) :xml)
            (do-tests ((format nil "Simple tests from file ~S" (file-namestring file-name))
                       (not verbose))
              (let ((form (or (read stream nil) (done))))
                (when verbose
                  (format t "~&~S" form))
                (cond ((and (consp form) (eq 'string= (car form))
                            (stringp (third form)))
                       (destructuring-bind (gen expected) (cdr form)
                         (let ((actual (eval gen)))
                           (unless (string= actual expected)
                             (list (format nil "~@<~:@_    ~2:I~S~:@_Expected: ~S~
                                                                 ~@:_  Actual: ~S~:>"
                                           form expected actual))))))
                      ((eval form) nil)
                      (t (list (format nil "~S returned NIL" form)))))))
        (setf (html-mode) html-mode)))))

(defun run-all-tests (&key verbose)
  "Runs all tests for CL-WHO and returns a true value iff all tests
succeeded.  VERBOSE is interpreted by the individual test suites."
  (let ((successp t))
    (macrolet ((run-test-suite (&body body)
                 `(unless (progn ,@body)
                    (setq successp nil))))
      (run-test-suite (simple-tests :verbose verbose)))
    (format t "~2&~:[Some tests failed~;All tests passed~]." successp)
    successp))