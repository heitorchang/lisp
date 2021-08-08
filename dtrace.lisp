;; Common Lisp a Gentle Introduction

;; implementation dependent
(defun fetch-arglist (fn)
  (arglist fn))

(defparameter *dtrace-print-length* 7)
(defparameter *dtrace-print-level* 4)
(defparameter *dtrace-print-circle* t)
(defparameter *dtrace-print-pretty* nil)
(defparameter *dtrace-print-array* *print-array*)

(defvar *traced-functions* nil)
(defvar *trace-level* 0)

(defmacro dtrace (&rest function-names)
  (if (null function-names)
      (list 'quote *traced-functions*)
      (list 'quote (mapcan #'dtrace1 function-names))))

(defun dtrace1 (name)
  (unless (symbolp name)
    (format *error-output* "~%~S is an invalid function name" name)
    (return-from dtrace1 nil))
  (unless (fboundp name)
    (format *error-output* "~%~S undefined function" name)
    (return-from dtrace1 nil))
  (eval `(untrace ,name))
  (duntrace1 name)
  (when (special-form-p name)
    (format *error-output*
            "~&can't trace ~S because it's a special form" name)
    (return-from dtrace1 nil))
  (if (macro-function name)
      (trace-macro name)
      (trace-function name))
  (setf *traced-functions* (nconc *traced-functions* (list name)))
  (list name))

(defun trace-function (name)
  (let* ((formal-arglist (fetch-arglist name))
         (old-defn (symbol-function name))
         (new-defn
           #'(lambda (&rest argument-list)
               (let ((result nil))
                 (display-function-entry name)
                 (let ((*trace-level* (1+ *trace-level*)))
                   (with-dtrace-printer-settings
                       (show-function-args argument-list formal-arglist))
                   (setf result (multiple-value-list
                                 (apply old-defn argument-list))))
                 (display-function-return name result)
                 (values-list result)))))
    (setf (get name 'original-definition) old-defn)
    (setf (get name 'traced-definition) new-defn)
    (setf (get name 'traced-type) 'defun)
    (setf (symbol-function name) new-defn)))

;; copying stopped here
