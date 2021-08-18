(defconstant +message+ "zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf enlpo pib slafml pvv bfwkj")
;;                      it is better to remain silent

(defconstant +letter-table+
  '(#\z #\i
    #\j #\t
    #\e #\s
    #\k #\b
    #\l #\e
    #\s #\r
    #\f #\o
    #\b #\d
    #\w #\u
    #\a #\m
    #\p #\a
    #\i #\n
    #\v #\l
    #\u #\h
    #\h #\f
    #\x #\g
    #\n #\p
    #\o #\k
    #\m #\v
    ))

(defun get-letter-or-underscore (letter)
  (let ((decoded (getf +letter-table+ letter)))
    (if (null decoded)
        #\space
        decoded)))

(defun decode ()
  (format t "~{~A~^~}"
          (mapcar (lambda (c) (get-letter-or-underscore c)) (coerce +message+ 'list))))

(format t "~%")
(decode)
