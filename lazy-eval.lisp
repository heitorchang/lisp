(defun natural-numbers-from (n)
  "Returns a lazy infinite list of nums from n"
  `(,n :promise natural-numbers-from ,(1+ n)))

(defun lazy-first (lst)
  (if (eql (first lst) :promise)
      (first (eval (rest lst)))
      (first lst)))

(defun lazy-rest (lst)
  (if (eql (first lst) :promise)
      (rest (eval (rest lst)))
      (rest lst)))

(defun lazy-count (n)
  (let ((seq (natural-numbers-from 0)))
    (dotimes (count n)
      (format t "~a~%" (lazy-first seq))
      (setf seq (lazy-rest seq)))))

(defun lazy-seq (count seq)
  (if (zerop count) t
      (progn
        (format t "Lazy seq: ~a~%" (lazy-first seq))
        (lazy-seq (1- count) (lazy-rest seq)))))
