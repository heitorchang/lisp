(defvar *prices* '((rice 6)
                   (beans 7)))

(defun sum-prices (shopping-list)
  (reduce #'+ (mapcar (lambda (name) (cadr (assoc name *prices*))) shopping-list)))

(defun sum-prices-buggy (shopping-list)
  ;; correct version uses cadr instead of caar
  (reduce #'+ (mapcar (lambda (name) (caar (assoc name *prices*))) shopping-list)))
