;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-who/packages.lisp,v 1.21 2009/01/26 11:10:49 edi Exp $

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

(in-package :cl-user)

(defpackage :cl-who
  (:use :cl)
  (:nicknames :who)
  #+(or :clasp :sbcl) (:shadow #:defconstant)
  #+:sb-package-locks (:lock t)
  (:export #:*attribute-quote-char*
           #:*empty-attribute-syntax*
           #:*escape-char-p*
           #:*prologue*
           #:*downcase-tokens-p*
           #:*html-no-indent-tags*
           #:*html-empty-tags*
           #:*html-empty-tag-aware-p*
           #:conc
           #:convert-attributes
           #:convert-tag-to-string-list
           #:esc
           #:escape-char
           #:escape-char-all
           #:escape-char-iso-8859-1
           #:escape-char-minimal
           #:escape-char-minimal-plus-quotes
           #:escape-string
           #:escape-string-all
           #:escape-string-iso-8859-1
           #:escape-string-minimal
           #:escape-string-minimal-plus-quotes
           #:fmt
           #:htm
           #:html-mode
           #:str
           #:with-html-output
           #:with-html-output-to-string))

(pushnew :cl-who *features*)
