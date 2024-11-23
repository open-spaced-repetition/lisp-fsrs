(defpackage lisp-fsrs.emacs
  (:use #:cl #:alexandria)
  (:import-from #:uiop #:string-prefix-p)
  (:nicknames #:fsrs.emacs)
  (:export #:translate-system))

(in-package #:lisp-fsrs.emacs)

(defparameter *prelude*
  '((require 'cl-lib)
    (require 'parse-time)
    (deftype timestamp () 'string)
    (defun now (&optional time)
      (format-time-string "%FT%TZ" time "UTC0"))
    (defun timestamp-difference (time-a time-b)
      (- (time-to-seconds (parse-iso8601-time-string time-a))
         (time-to-seconds (parse-iso8601-time-string time-b))))
    (defun timestamp+ (time amount unit)
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
    (let ((*mappings* (set-difference *mappings* (mapcar (compose #'nreverse #'first) (nth-value 3 (parse-ordinary-lambda-list lambda-list))) :key #'car)))
      (call-next-method car (list* name lambda-list body)))))

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
           (translated-name (car name-and-options)))
      (list* 'cl-defstruct name-and-options
             (loop :for slot :in slots
                   :for (slot-name . slot-options) := (ensure-cons slot)
                   :do (setf (assoc-value *mappings* (symbolicate name '#:- slot-name)) (symbolicate translated-name '#:- slot-name))
                   :collect (cons slot-name (translate slot-options))
                   :finally
                      (setf (assoc-value *mappings* name) translated-name
                            (assoc-value *mappings* (symbolicate name '#:-p)) (symbolicate translated-name '#:-p)
                            (assoc-value *mappings* (symbolicate '#:make- name)) (symbolicate '#:make- translated-name)
                            (assoc-value *mappings* (symbolicate '#:copy- name)) (symbolicate '#:copy- translated-name)))))))

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
              (return-from translate-form clause)
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

;;; Commentary:

;; This file was auto-transpiled from ~{~A~^, ~}.

;;; Code:
" (mapcar (compose #'pathname-filename #'asdf:component-pathname) (asdf:component-children system)))
    (translate-component system output)
    (format output ";;; ~A.~A ends here~%" (pathname-name file) (pathname-type file))))
