(defpackage lisp-fsrs.emacs
  (:use #:cl #:alexandria)
  (:import-from #:uiop #:string-prefix-p)
  (:nicknames #:fsrs.emacs)
  (:export #:translate-system))

(in-package #:lisp-fsrs.emacs)

(defparameter *prelude*
  '((require 'cl-lib)
    (require 'cl-generic)
    (require 'parse-time)
    (deftype timestamp ()
      "ISO 8601 UTC timestamp string type.

Represents time values in `YYYY-MM-DDTHH:MM:SSZ' format. Used
throughout FSRS for all date/time tracking related to card scheduling
and review logging."
      'string)
    (defun now (&optional time)
      "Get current UTC time as TIMESTAMP string.

When TIME is non-nil (accepts time value or nil), format that instead
of current time. Returns string formatted according to ISO 8601 with
UTC timezone."
      (format-time-string "%FT%TZ" time "UTC0"))
    (defun timestamp-difference (time-a time-b)
      "Calculate difference between two timestamps in seconds.

TIME-A and TIME-B must both be TIMESTAMP strings. Returns
floating-point number representing TIME-A minus TIME-B in seconds.
Handles ISO 8601 parsing."
      (- (time-to-seconds (parse-iso8601-time-string time-a))
         (time-to-seconds (parse-iso8601-time-string time-b))))
    (defun timestamp+ (time amount unit)
      "Create new TIMESTAMP by adding time units.

TIME is base TIMESTAMP string. AMOUNT is number of units to add. UNIT
is one of :sec/:minute/:hour/:day keyword specifying time unit.
Returns new ISO 8601 string calculated by adding AMOUNT × UNIT's
seconds to TIME."
      (now
       (+ (time-to-seconds (parse-iso8601-time-string time))
          (* amount
             (ecase unit
               (:sec 1)
               (:minute #.local-time::+seconds-per-minute+)
               (:hour #.local-time::+seconds-per-hour+)
               (:day #.local-time::+seconds-per-day+))))))))

(defparameter *postlude*
  '((provide 'fsrs)))

(defvar *mappings*
  (let (mappings)
    (do-external-symbols (symbol :cl mappings)
      (when (fboundp symbol)
        (let ((cl-symbol (symbolicate '#:cl- symbol)))
          (if (slynk:eval-in-emacs `(fboundp ',cl-symbol))
              (push (cons symbol cl-symbol) mappings)
              (unless (slynk:eval-in-emacs `(fboundp ',symbol))
                (warn "Function/macro ~A is not available in Emacs Lisp." symbol))))))))

(defun translate-docstring (docstring)
  (str:replace-using
   (loop :for (from . to) :in *mappings*
         :nconc (list (format nil "(?<=\\s)~A(?=\\s)" (ppcre:quote-meta-chars (symbol-name from))) (symbol-name to)))
   docstring :regex t))

(defgeneric translate-form (car cdr))

(defun translate (object)
  (typecase object
    (null object)
    (proper-list (translate-form (car object) (cdr object)))
    (symbol (or (assoc-value *mappings* object) object))
    (t object)))

(defmethod translate-form ((car symbol) cdr)
  (cons (or (assoc-value *mappings* car) car) (mapcar #'translate cdr)))

(defmethod translate-form (car cdr)
  (mapcar #'translate (cons car cdr)))

(defmethod translate-form ((car (eql 'defpackage)) cdr)
  (throw 'skip nil))

(defmethod translate-form ((car (eql 'in-package)) cdr)
  (throw 'skip nil))

(defmethod translate-form ((car (eql 'function)) cdr)
  (cons car (mapcar #'translate cdr)))

(defun translate-definition (args &optional var)
  (let* ((name (car args))
         (translated-name
           (if-let ((translated-name (assoc-value (symbol-value var) name)))
             translated-name
             (let* ((name (symbol-name name))
                    (name (if (string-prefix-p "+" name) (string-trim "+" name) name))
                    (prefix (string '#:fsrs)))
               (intern
                (if (string-prefix-p prefix name)
                    name (format nil "~A-~A" prefix name)))))))
    (when var
      (unless (eq name translated-name)
        (setf (assoc-value (symbol-value var) name) translated-name)))
    (cons translated-name (cdr args))))

(defmethod translate-form ((car (eql 'defun)) cdr)
  (destructuring-bind (name lambda-list &rest body) cdr
    (setf name (first (translate-definition cdr '*mappings*)))
    (let ((*mappings* (set-difference
                       *mappings*
                       (mapcar (compose #'nreverse #'first)
                               (nth-value 3 (parse-ordinary-lambda-list lambda-list :allow-specializers t)))
                       :key #'car)))
      (when (stringp (car body))
        (setf (car body) (translate-docstring (car body))))
      (call-next-method car (list* name lambda-list body)))))

(defmethod translate-form ((car (eql 'defgeneric)) cdr)
  (when-let ((documentation (assoc-value (cddr cdr) :documentation)))
    (setf (car documentation) (translate-docstring (car documentation))))
  (cons 'cl-defgeneric (cdr (translate-form 'defun cdr))))

(defmethod translate-form ((car (eql 'defmethod)) cdr)
  (cons 'cl-defmethod (cdr (translate-form 'defun cdr))))

(defmethod translate-form ((car (eql 'deftype)) cdr)
  (let ((cdr (translate-definition cdr '*mappings*))
        (*mappings* (remove 'member *mappings* :key #'car)))
    (call-next-method car cdr)))

(defmethod translate-form ((car (eql 'defconstant)) cdr)
  (translate-form 'defconst (translate-definition cdr '*mappings*)))

(defmethod translate-form ((car (eql 'define-constant)) cdr)
  (translate-form 'defconst (subseq (translate-definition cdr '*mappings*) 0 2)))

(defmethod translate-form ((car (eql 'defstruct)) cdr)
  (destructuring-bind (name-and-options &rest slots) cdr
    (let* ((name-and-options (ensure-cons name-and-options))
           (name (car name-and-options))
           (name-and-options (translate-definition name-and-options))
           (translated-name (car name-and-options))
           (documentation (when (stringp (car slots)) (pop slots))))
      (loop :for keyword :in '(:constructor :copier)
            :for prefix :in '(#:make- #:copy-)
            :for symbol-cons := (assoc-value (cdr name-and-options) keyword)
            :for function := (symbolicate '#:fsrs- prefix name)
            :if symbol-cons
              :when (car symbol-cons)
                :do (setf (car symbol-cons)
                          (setf (assoc-value *mappings* (car symbol-cons))
                                (symbolicate '#:fsrs- (substitute #\- #\% (symbol-name (car symbol-cons))))))
            :end
            :else
              :do (push (list keyword function) (cdr name-and-options))
            :do (setf (assoc-value *mappings* (symbolicate prefix name)) function))
      `(cl-defstruct
        ,(translate name-and-options)
        ,@(when documentation (list documentation))
        ,@(loop :for slot :in slots
                :for (slot-name . slot-options) := (ensure-cons slot)
                :do (setf (assoc-value *mappings* (symbolicate name '#:- slot-name)) (symbolicate translated-name '#:- slot-name))
                :collect (cons slot-name (translate slot-options))
                :finally
                   (setf (assoc-value *mappings* name) translated-name
                         (assoc-value *mappings* (symbolicate name '#:-p)) (symbolicate translated-name '#:-p)))))))

(defmethod translate-form ((car (eql 'coerce)) args)
  (destructuring-bind (object type) args
    `(cl-coerce
      ,(translate object)
      ,(translate
        (if (constantp type)
            (multiple-value-bind (expansion expandedp)
                (introspect-environment:typexpand
                 (mapcar
                  (compose (rcurry #'find-symbol #.(find-package '#:lisp-fsrs)) #'symbol-name)
                  (ensure-list (eval type))))
              (if expandedp
                  `',(if (and (listp expansion) (null (cdr expansion))) (car expansion) expansion)
                  type))
            type)))))

(defmethod translate-form ((car (eql 'simple-array)) cdr)
  'vector)

(defmethod translate-form ((car (eql 'values)) cdr)
  (case (length cdr)
    (0 nil)
    (1 (translate (first cdr)))
    (t (call-next-method))))

(defmethod translate-form ((car (eql 'declare)) cdr)
  (translate-form
   'cl-declare
   (case (length cdr)
     (0 nil)
     (1 (let ((clause (first cdr)))
          (if (eq (car clause) 'ignore)
              (return-from translate-form (translate clause))
              (list clause))))
     (t cdr))))

(setf (assoc-value *mappings* 'single-float) 'float
      (assoc-value *mappings* 'double-float) 'float
      (assoc-value *mappings* 'non-negative-single-float) 'float
      (assoc-value *mappings* 'non-negative-fixnum) 'fixnum)

(defun write-toplevel (form output)
  (let ((form (translate form)))
    (write-char #\Newline output)
    (write form :stream output :pretty t :readably t :case :downcase)
    (write-char #\Newline output)))

(defun translate-file (input output)
  (handler-case
      (loop
        (catch 'skip
          (write-toplevel (read input) output)))
    (end-of-file ())))

(defun translate-component (component output)
  (loop :for form :in *prelude*
        :do (write-toplevel (translate-form (car form) (cdr form)) output))
  (loop :for file :in (asdf:component-children component)
        :do (with-open-file (input (asdf:component-pathname file)) (translate-file input output)))
  (loop :for form :in *postlude*
        :do (write-toplevel (translate-form (car form) (cdr form)) output)))

(defun pathname-filename (pathname)
  (format nil "~A~@[.~A~]" (pathname-name pathname) (pathname-type pathname)))

(defun translate-system (&optional
                           (system (asdf:find-system '#:lisp-fsrs))
                           (file (merge-pathnames #P"fsrs.el" (asdf:system-source-directory system)))
                         &aux
                           (*package* #.*package*))
  (translate-component system (make-string-output-stream))
  (with-open-file (output file :direction :output :if-exists :supersede)
    (format output ";;; fsrs.el --- Emacs Lisp Package for FSRS -*- lexical-binding: t -*-

;; Copyright (C) 2025 Open Spaced Repetition

;; Author: Open Spaced Repetition
;; Maintainer: Open Spaced Repetition
;; Version: 5.0
;; Package-Requires: ((emacs \"25.1\"))
;; URL: https://github.com/open-spaced-repetition/lisp-fsrs
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the \"Software\"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is furnished to do
;; so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; FSRS is a spaced repetition algorithm that optimizes review scheduling
;; by adapting to individual memory patterns, outperforming SM-2.

;;; Code:
" (mapcar (compose #'pathname-filename #'asdf:component-pathname) (asdf:component-children system)))
    (translate-component system output)
    (format output ";;; ~A.~A ends here~%" (pathname-name file) (pathname-type file))))
