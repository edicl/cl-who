;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:CL-WHO; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-who/util.lisp,v 1.4 2009/01/26 11:10:49 edi Exp $

;;; Copyright (c) 2003-2009, Dr. Edmund Weitz. All rights reserved.

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

(in-package :cl-who)

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:with-unique-names))

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'with-rebinding)
          (macro-function 'lw:rebinding)))

#-:lispworks
(defmacro with-rebinding (bindings &body body)
  "WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

;; TODO...
#+(or)
(defun apply-to-tree (function test tree)
  (declare (optimize speed space))
  (declare (type function function test))
  "Applies FUNCTION recursively to all elements of the tree TREE \(not
only leaves) which pass TEST."
  (cond
    ((funcall test tree)
      (funcall function tree))
    ((consp tree)
      (cons
       (apply-to-tree function test (car tree))
       (apply-to-tree function test (cdr tree))))
    (t tree)))

(defmacro n-spaces (n)
  "A string with N spaces - used by indentation."
  `(make-array ,n
               :element-type 'base-char
               :displaced-to +spaces+
               :displaced-index-offset 0))

(declaim (inline escape-char))
(defun escape-char (char &key (test *escape-char-p*))
  (declare (optimize speed))
  "Returns an escaped version of the character CHAR if CHAR satisfies
the predicate TEST.  Always returns a string."
  (if (funcall test char)
    (case char
      (#\< "&lt;")
      (#\> "&gt;")
      (#\& "&amp;")
      (#\' "&#039;")
      (#\" "&quot;")
      (t (format nil (if (eq *html-mode* :xml) "&#x~x;" "&#~d;")
                 (char-code char))))
    (make-string 1 :initial-element char)))

(defun escape-string (string &key (test *escape-char-p*))
  (declare (optimize speed))
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (let ((first-pos (position-if test string))
        (format-string (if (eq *html-mode* :xml) "&#x~x;" "&#~d;")))
    (if (not first-pos)
      ;; nothing to do, just return STRING
      string
      (with-output-to-string (s)
        (loop with len = (length string)
              for old-pos = 0 then (1+ pos)
              for pos = first-pos
                  then (position-if test string :start old-pos)
              ;; now the characters from OLD-POS to (excluding) POS
              ;; don't have to be escaped while the next character has to
              for char = (and pos (char string pos))
              while pos
              do (write-sequence string s :start old-pos :end pos)
                 (case char
                   ((#\<)
                     (write-sequence "&lt;" s))
                   ((#\>)
                     (write-sequence "&gt;" s))
                   ((#\&)
                     (write-sequence "&amp;" s))
                   ((#\')
                     (write-sequence "&#039;" s))
                   ((#\")
                     (write-sequence "&quot;" s))
                   (otherwise
                     (format s format-string (char-code char))))
              while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))

(defun minimal-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL functions to determine
whether CHAR must be escaped."
  (find char "<>&"))

(defun escape-char-minimal (char)
  "Escapes only #\<, #\>, and #\& characters."
  (escape-char char :test #'minimal-escape-char-p))

(defun escape-string-minimal (string)
  "Escapes only #\<, #\>, and #\& in STRING."
  (escape-string string :test #'minimal-escape-char-p))

(defun minimal-plus-quotes-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL-PLUS-QUOTES functions to
determine whether CHAR must be escaped."
  (find char "<>&'\""))

(defun escape-char-minimal-plus-quotes (char)
  "Like ESCAPE-CHAR-MINIMAL but also escapes quotes."
  (escape-char char :test #'minimal-plus-quotes-escape-char-p))

(defun escape-string-minimal-plus-quotes (string)
  "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
  (escape-string string :test #'minimal-plus-quotes-escape-char-p))

(defun iso-8859-1-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (find char "<>&'\"")
      (> (char-code char) 255)))

(defun escape-char-iso-8859-1 (char)
  "Escapes characters that aren't defined in ISO-8859-9."
  (escape-char char :test #'iso-8859-1-escape-char-p))

(defun escape-string-iso-8859-1 (string)
  "Escapes all characters in STRING which aren't defined in ISO-8859-1."
  (escape-string string :test #'iso-8859-1-escape-char-p))

(defun non-7bit-ascii-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (find char "<>&'\"")
      (> (char-code char) 127)))

(defun escape-char-all (char)
  "Escapes characters which aren't in the 7-bit ASCII character set."
  (escape-char char :test #'non-7bit-ascii-escape-char-p))

(defun escape-string-all (string)
  "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
  (escape-string string :test #'non-7bit-ascii-escape-char-p))

(defun extract-declarations (forms)
  "Given a FORM, the declarations - if any - will be extracted
   from the head of the FORM, and will return two values the declarations,
   and the remaining of FORM"
  (loop with declarations
        for forms on forms
        for form = (first forms)
        while (and (consp form)
                   (eql (first form) 'cl:declare))
        do (push form declarations)
        finally (return (values (nreverse declarations) forms))))

(defun same-case-p (string)
  "Test if all characters of a string are in the same case."
  (or (every #'(lambda (c) (or (not (alpha-char-p c)) (lower-case-p c))) string)
      (every #'(lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) string)))

(defun maybe-downcase (symbol)
  (let ((string (string symbol)))
    (if (and *downcase-tokens-p* (same-case-p string))
        (string-downcase string)
        string)))
