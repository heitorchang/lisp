(defun my-copy (lst)
  (if (null lst) '()
      (cons (first lst) (my-copy (rest lst)))))

(defun my-reverse (lst accum)
  (if (null lst) accum
      (my-reverse (rest lst) (cons (first lst) accum))))

(defun int-append (lst1 lst2)
  (if (null lst1) lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

(defun int-reverse (lst)
  (if (null lst) '()
      (int-append (int-reverse (cdr lst))
                  (list (car lst)))))

(defun sub-first (new old lst)
  (cond ((null lst) '())
        ((eql (car lst) old) (cons new (cdr lst)))
        (t (cons (car lst)
                 (sub-first new old (cdr lst))))))

(defun sub-all (new old lst)
  (cond ((null lst) '())
        ((eql (car lst) old) (cons new (sub-all new old (cdr lst))))
        (t (cons (car lst)
                 (sub-all new old (cdr lst))))))

(defun int-identity (obj)
  obj)
