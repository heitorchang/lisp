;; pi estimate: 4 - 4/3 + 4/5 - 4/7 + 4/9
;; compiler must be optimized for speed or stack will overflow
;; (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(defun digits-match (target estimate)
  (let ((estimate-string (write-to-string estimate)))
    (if (< (length estimate-string) (length target))
        nil
        (string= target (subseq estimate-string 0 (length target))))))

(defun find-pi-2-term (index)
  "Compute the sequence term with the given index"
  (let ((sign (if (= 0 (mod index 2))
                  1
                  -1)))
    (* sign (/ 4 (+ 1 (* index 2))))))

(defun find-pi-2-loop (index estimate)
  (if (digits-match "3.14159" estimate)
      ;; index has already been increased, correct it
      (format t "~a ~a~%" (find-pi-2-term (- index 1)) estimate)  
      (find-pi-2-loop (1+ index) (+ (find-pi-2-term index) estimate))))

(defun find-pi-2 ()
  (find-pi-2-loop 0 0L0))

;; 4/272241 3.141599999994786D0

