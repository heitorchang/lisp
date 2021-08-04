;; double entry accounting

(defvar *transactions* nil)
(defconstant +transactions-file+ "/home/heitor/lisp/alispie/transactions.txt")

(defstruct transaction
  description
  amount
  debit
  credit
  date)

(defun save-transactions ()
  (with-open-file (out +transactions-file+
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *transactions* out))))

(defun load-transactions ()
  (with-open-file (in +transactions-file+)
    (with-standard-io-syntax
      (setf *transactions* (read in)))))

(defun add-transaction (description amount debit credit date)
  (push (make-transaction
         :description description
         :amount amount
         :debit debit
         :credit credit
         :date date)
        *transactions*))

(defun sum-transactions (transaction-query account-name)
  (reduce #'+
          (mapcar #'transaction-amount
                  (remove-if-not
                   (lambda (txn) (eql (funcall transaction-query txn) account-name)) *transactions*))))

(defun account-balance (account-name)
  (let ((debits (sum-transactions #'transaction-debit account-name))
        (credits (sum-transactions #'transaction-credit account-name)))
    ;; check account sign +/-1
    (- debits credits)))

;; start up
(load-transactions)
