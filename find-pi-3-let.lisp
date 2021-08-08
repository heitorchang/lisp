(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(defun digits-match (target estimate)
  (let ((estimate-string (write-to-string estimate)))
    (if (< (length estimate-string) (length target))
        nil
        (string= target (subseq estimate-string 0 (length target))))))

(defun find-pi-3-term (index)
  "Compute the sequence term with the given index"
  (let ((sign (if (= 0 (mod index 2))
                  1
                  -1)))
    (* sign (/ 4 (+ 1 (* index 2))))))

(defun find-pi-3-loop (index estimate)
  (let ((new-index (+ index 1))
        (new-estimate (+ estimate (find-pi-3-term index))))
    (if (digits-match "3.14159" new-estimate)
        (format t "~a ~a~%"  (find-pi-3-term index) new-estimate)
        (find-pi-3-loop new-index new-estimate))))

(defun find-pi-3 ()
  (find-pi-3-loop 0 0L0))
