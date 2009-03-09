;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:CL-WHO; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-who/who.lisp,v 1.35 2007/08/24 08:01:37 edi Exp $

;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.

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

(defmacro n-spaces (n)
  "A string with N spaces - used by indentation."
  `(make-array ,n
               :element-type 'base-char
               :displaced-to +spaces+
               :displaced-index-offset 0))

(defun html-mode ()
  "Returns the current HTML mode. :SGML for (SGML-)HTML and
:XML for XHTML."
  *html-mode*)

(defun (setf html-mode) (mode)
  "Sets the output mode to XHTML or \(SGML-)HTML.  MODE can be
:SGML for HTML or :XML for XHTML."
  (ecase mode
    ((:sgml)
     (setf *html-mode* :sgml
           *empty-tag-end* ">"
           *prologue* "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"))
    ((:xml)
     (setf *html-mode* :xml
           *empty-tag-end* " />"
           *prologue* "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))))

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

(flet ((minimal-escape-char-p (char) (find char "<>&")))
  (defun escape-char-minimal (char)
    "Escapes only #\<, #\>, and #\& characters."
    (escape-char char :test #'minimal-escape-char-p))
  (defun escape-string-minimal (string)
    "Escapes only #\<, #\>, and #\& in STRING."
    (escape-string string :test #'minimal-escape-char-p)))

(flet ((minimal-plus-quotes-escape-char-p (char) (find char "<>&'\"")))
  (defun escape-char-minimal-plus-quotes (char)
    "Like ESCAPE-CHAR-MINIMAL but also escapes quotes."
    (escape-char char :test #'minimal-plus-quotes-escape-char-p))
  (defun escape-string-minimal-plus-quotes (string)
    "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
    (escape-string string :test #'minimal-plus-quotes-escape-char-p)))

(flet ((iso-8859-1-escape-char-p (char)
         (or (find char "<>&'\"")
             (> (char-code char) 255))))
  (defun escape-char-iso-8859-1 (char)
    "Escapes characters that aren't defined in ISO-8859-9."
    (escape-char char :test #'iso-8859-1-escape-char-p))
  (defun escape-string-iso-8859-1 (string)
    "Escapes all characters in STRING which aren't defined in ISO-8859-1."
    (escape-string string :test #'iso-8859-1-escape-char-p)))

(defun escape-string-iso-8859 (string)
  "Identical to ESCAPE-STRING-8859-1.  Kept for backward compatibility."
  (escape-string-iso-8859-1 string))

(flet ((non-7bit-ascii-escape-char-p (char)
         (or (find char "<>&'\"")
             (> (char-code char) 127))))
  (defun escape-char-all (char)
    "Escapes characters which aren't in the 7-bit ASCII character set."
    (escape-char char :test #'non-7bit-ascii-escape-char-p))
  (defun escape-string-all (string)
    "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
    (escape-string string :test #'non-7bit-ascii-escape-char-p)))

(defun process-tag (sexp body-fn)
  (declare (optimize speed space))
  "Returns a string list corresponding to the `HTML' \(in CL-WHO
syntax) in SEXP.  Uses the generic function CONVERT-TO-STRING-LIST
internally.  Utility function used by TREE-TO-TEMPLATE."
  (let (tag attr-list body)
    (cond
      ((keywordp sexp)
       (setq tag sexp))
      ((atom (first sexp))
       (setq tag (first sexp))
       ;; collect attribute/value pairs into ATTR-LIST and tag body (if
       ;; any) into BODY
       (loop for rest on (cdr sexp) by #'cddr
             if (keywordp (first rest))
               collect (cons (first rest) (second rest)) into attr
             else
               do (progn (setq attr-list attr)
                         (setq body rest)
                         (return))
             finally (setq attr-list attr)))
      ((listp (first sexp))
       (setq tag (first (first sexp)))
       (loop for rest on (cdr (first sexp)) by #'cddr
             if (keywordp (first rest))
               collect (cons (first rest) (second rest)) into attr
             finally (setq attr-list attr))
       (setq body (cdr sexp))))
    (convert-tag-to-string-list tag attr-list body body-fn)))

(defun convert-attributes (attr-list)
  "Helper function for CONVERT-TAG-TO-STRING-LIST which converts the
alist ATTR-LIST of attributes into a list of strings and/or Lisp
forms."
  (declare (optimize speed space))
  (loop with =var= = (gensym)
        with attribute-quote = (string *attribute-quote-char*)
        for (orig-attr . val) in attr-list
        for attr = (if *downcase-tokens-p*
                     (string-downcase orig-attr)
                     (string orig-attr))
        unless (null val) ;; no attribute at all if VAL is NIL
          if (constantp val)
            if (and (eq *html-mode* :sgml) (eq val t)) ; special case for SGML
              nconc (list " " attr)
            else
              nconc (list " "
                          ;; name of attribute
                          attr
                          (format nil "=~C" *attribute-quote-char*)
                          ;; value of attribute
                          (cond ((stringp val)
                                 ;; a string, just use it - this case is
                                 ;; actually not necessary because of
                                 ;; the last case
                                 val)
                                ((eq val t)
                                 ;; VAL is T, use attribute's name
                                 attr)
                                (t
                                 ;; constant form, PRINC it -
                                 ;; EVAL is OK here because of CONSTANTP
                                 (format nil "~A" (eval val))))
                          attribute-quote)
            end
          else
            ;; do the same things as above but at runtime
            nconc (list `(let ((,=var= ,val))
                          (cond ((null ,=var=))
                                ((eq ,=var= t) 
                                 ,(case *html-mode*
                                        (:sgml
                                         `(htm ,(format nil " ~A" attr)))
                                        ;; otherwise default to :xml mode
                                        (t
                                         `(htm ,(format nil " ~A=~C~A~C"
                                                        attr
                                                        *attribute-quote-char*
                                                        attr
                                                        *attribute-quote-char*)))))
                                (t
                                 (htm ,(format nil " ~A=~C" attr *attribute-quote-char*)
                                      (str ,=var=)
                                      ,attribute-quote)))))))

(defgeneric convert-tag-to-string-list (tag attr-list body body-fn)
  (:documentation "Used by PROCESS-TAG to convert `HTML' into a list
of strings.  TAG is a keyword symbol naming the outer tag, ATTR-LIST
is an alist of its attributes \(the car is the attribute's name as a
keyword, the cdr is its value), BODY is the tag's body, and BODY-FN is
a function which should be applied to BODY.  The function must return
a list of strings or Lisp forms."))

(defmethod convert-tag-to-string-list (tag attr-list body body-fn)
  "The standard method which is not specialized.  The idea is that you
can use EQL specializers on the first argument."
  (declare (optimize speed space))
  (let ((tag (if *downcase-tokens-p* (string-downcase tag) (string tag))))
    (nconc
     (if *indent*
       ;; indent by *INDENT* spaces
       (list +newline+ (n-spaces *indent*)))
     ;; tag name
     (list "<" tag)
     ;; attributes
     (convert-attributes attr-list)
     ;; body
     (if body
       (append
        (list ">")
        ;; now hand over the tag's body to TREE-TO-TEMPLATE, increase
        ;; *INDENT* by 2 if necessary
        (if *indent*
          (let ((*indent* (+ 2 *indent*)))
            (funcall body-fn body))
          (funcall body-fn body))
        (if *indent*
          ;; indentation
          (list +newline+ (n-spaces *indent*)))
        ;; closing tag
        (list "</" tag ">"))
       ;; no body, so no closing tag unless defined in *HTML-EMPTY-TAGS*
       (if (or (not *html-empty-tag-aware-p*)
               (member tag *html-empty-tags* :test #'string-equal))
         (list *empty-tag-end*)
         (list ">" "</" tag ">"))))))

(defun apply-to-tree (function test tree)
  (declare (optimize speed space))
  (declare (type function function test))
  "Apply FUNCTION recursively to all elements of the tree TREE \(not
only leaves) which pass TEST."
  (cond
    ((funcall test tree)
      (funcall function tree))
    ((consp tree)
      (cons
       (apply-to-tree function test (car tree))
       (apply-to-tree function test (cdr tree))))
    (t tree)))

(defun replace-htm (tree transformation)
  (declare (optimize speed space))
  "Replace all subtrees of TREE starting with the symbol HTM with the
same subtree after TRANSFORMATION has been applied to it. Utility
function used by TREE-TO-TEMPLATE and TREE-TO-COMMANDS-AUX."
  (apply-to-tree #'(lambda (element)
                     (cons 'htm (funcall transformation (cdr element))))
                 #'(lambda (element)
                     (and (consp element)
                          (eq (car element) 'htm)))
                 tree))

(defun tree-to-template (tree)
  "Transforms an HTML tree into an intermediate format - mainly a
flattened list of strings. Utility function used by TREE-TO-COMMANDS-AUX."
  (loop for element in tree
        nconc (cond ((or (keywordp element)
                         (and (listp element)
                              (keywordp (first element)))
                         (and (listp element)
                              (listp (first element))
                              (keywordp (first (first element)))))
                     ;; normal tag
                     (process-tag element #'tree-to-template))
                    ((listp element)
                     ;; most likely a normal Lisp form - check if we
                     ;; have nested HTM subtrees
                     (list
                      (replace-htm element #'tree-to-template)))
                    (t
                     (if *indent*
                       (list +newline+ (n-spaces *indent*) element)
                       (list element))))))

(defun string-list-to-string (string-list)
  (declare (optimize speed space))
  "Concatenates a list of strings to one string."
  ;; note that we can't use APPLY with CONCATENATE here because of
  ;; CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (dolist (string string-list)
      (incf total-size (length string)))
    (let ((result-string (make-sequence 'simple-string total-size))
          (curr-pos 0))
      (dolist (string string-list)
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun conc (&rest string-list)
  "Concatenates all arguments which should be string into one string."
  (funcall #'string-list-to-string string-list))

(defun tree-to-commands-aux (tree stream)
  (declare (optimize speed space))
  "Transforms the intermediate representation of an HTML tree into
Lisp code to print the HTML to STREAM. Utility function used by
TREE-TO-COMMANDS."
  (let ((in-string t)
        collector
        string-collector)
    (flet ((emit-string-collector ()
             "Generate a WRITE-STRING statement for what is currently
in STRING-COLLECTOR."
             (list 'write-string
                   (string-list-to-string (nreverse string-collector))
                   stream))
           (tree-to-commands-aux-internal (tree)
             "Same as TREE-TO-COMMANDS-AUX but with closed-over STREAM
for REPLACE-HTM."
             (tree-to-commands-aux tree stream)))
      (unless (listp tree)
        (return-from tree-to-commands-aux tree))
      (loop for element in tree
            do (cond ((and in-string (stringp element))
                       ;; this element is a string and the last one
                       ;; also was (or this is the first element) -
                       ;; collect into STRING-COLLECTOR
                       (push element string-collector))
                     ((stringp element)
                       ;; the last one wasn't a string so we start
                       ;; with an empty STRING-COLLECTOR
                       (setq string-collector (list element)
                             in-string t))
                     (string-collector
                       ;; not a string but STRING-COLLECTOR isn't
                       ;; empty so we have to emit the collected
                       ;; strings first
                       (push (emit-string-collector) collector)
                       (setq in-string nil
                             string-collector '())
                       ;; collect this element but walk down the
                       ;; subtree first
                       (push (replace-htm element #'tree-to-commands-aux-internal)
                             collector))
                     (t
                       ;; not a string and empty STRING-COLLECTOR
                       (push (replace-htm element #'tree-to-commands-aux-internal)
                             collector)))
            finally (return (if string-collector
                              ;; finally empty STRING-COLLECTOR if
                              ;; there's something in it
                              (nreverse (cons (emit-string-collector)
                                              collector))
                              (nreverse collector)))))))

(defun tree-to-commands (tree stream &optional prologue)
  (declare (optimize speed space))
  "Transforms an HTML tree into code to print the HTML to STREAM."
  ;; use TREE-TO-TEMPLATE, then TREE-TO-COMMANDS-AUX, and finally
  ;; replace the special symbols ESC, STR, FMT, and HTM
  (apply-to-tree #'(lambda (x)
                     (case (first x)
                       ((esc)
                        ;; (ESC form ...)
                        ;; --> (LET ((RESULT form))
                        ;;       (WHEN RESULT
                        ;;         (WRITE-STRING (ESCAPE-STRING RESULT STREAM))))
                        (let ((result (gensym)))
                          `(let ((,result ,(second x)))
                             (when ,result (write-string (escape-string ,result) ,stream)))))
                       ((str)
                        ;; (STR form ...)
                        ;; --> (LET ((RESULT form))
                        ;;       (WHEN RESULT (PRINC RESULT STREAM)))
                        (let ((result (gensym)))
                          `(let ((,result ,(second x)))
                             (when ,result (princ ,result ,stream)))))
                       ((fmt)
                        ;; (FMT form*) --> (FORMAT STREAM form*)
                        (list* 'format stream (rest x)))))
                 #'(lambda (x)
                     (and (consp x)
                          (member (first x)
                                  '(esc str fmt)
                                  :test #'eq)))
                 ;; wrap PROGN around the HTM forms
                 (apply-to-tree (constantly 'progn)
                                #'(lambda (x)
                                    (and (atom x)
                                         (eq x 'htm)))
                                (tree-to-commands-aux
                                 (if prologue
                                   (list* 'htm prologue +newline+
                                          (tree-to-template tree))
                                   (cons 'htm (tree-to-template tree)))
                                 stream))))

(defmacro with-html-output ((var &optional stream
                                 &key prologue
                                      ((:indent *indent*) *indent*))
                            &body body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code to write the corresponding HTML as strings to VAR -
which should either hold a stream or which'll be bound to STREAM if
supplied."
  (when (and *indent*
             (not (integerp *indent*)))
    (setq *indent* 0))
  (when (eq prologue t)
    (setq prologue *prologue*))
  `(let ((,var ,(or stream var)))
    ,(tree-to-commands body var prologue)))

(defmacro with-html-output-to-string ((var &optional string-form
                                           &key (element-type ''character)
                                                prologue
                                                indent)
                                      &body body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code which creates the corresponding HTML as a string."
  `(with-output-to-string (,var ,string-form
                                #-(or :ecl :cmu :sbcl) :element-type
                                #-(or :ecl :cmu :sbcl) ,element-type)
    (with-html-output (,var nil :prologue ,prologue :indent ,indent)
      ,@body)))

(defmacro show-html-expansion ((var &optional stream
                                    &key prologue
                                         ((:indent *indent*) *indent*))
                               &body body)
  "Show the macro expansion of WITH-HTML-OUTPUT."
  (when (and *indent*
             (not (integerp *indent*)))
    (setq *indent* 0))
  (when (eq prologue t)
    (setq prologue *prologue*))
  `(pprint '(let ((,var ,(or stream var)))
             ,(tree-to-commands body var prologue))))

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-who/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-who
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
