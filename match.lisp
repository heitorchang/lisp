(defpackage "MATCH"
  (:use "COMMON-LISP"))

(in-package "MATCH")

(defun variablep (s)
  (and (symbolp s)
       (char= (char (symbol-name s) 0) #\?)))

(defun match-element (e1 e2)
  (cond ((eql e1 e2) t)
        ((or (eql e1 '?) (eql e2 '?)) t)
        ((variablep e1) (list e1 e2))
        ((variablep e2) (list e2 e1))
        (t nil)))

(defun dont-care (arg)
  (if (eql arg '?)
      t
      nil))
      
