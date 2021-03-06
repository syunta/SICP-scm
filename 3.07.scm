(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? passwd pw)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
      (lambda x "Incorrect password")))
  dispatch)

(define (make-joint acc passwd new-passwd)
  (if (not (equal? ((acc passwd 'withdraw) 0) "Incorrect password"))
    (lambda (pw m)
      (if (eq? pw new-passwd)
        (acc passwd m)
        (lambda x "Incorrect password")))
    (error "Incorrect password -- MAKE-JOINT"
           passwd)))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(define hoge-acc (make-joint paul-acc 'rosebud 'hoge))

(define (main args)
  (print ((paul-acc 'rosebud 'withdraw) 30))
  (print ((peter-acc 'open-sesame 'withdraw) 10))
  (print ((paul-acc 'rosebud 'deposit) 30))
  (print ((peter-acc 'open-sesame 'deposit) 10))
  (print ((hoge-acc 'hoge 'withdraw) 20))
  (print ((peter-acc 'open-sesame 'withdraw) 10)))
