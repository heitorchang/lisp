(defun sum (n1 n2)
  (if (zerop n1) n2
      (sum (1- n1) (1+ n2))))

(defun sum2 (n1 n2)
  (if (zerop n1) n2
      (1+ (sum2 (1- n1) n2))))
