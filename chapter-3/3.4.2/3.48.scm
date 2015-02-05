; デッドロックを回避する理由
; 複数の共有資源へのアクセス順を統一することで回避できる

(define generate-id
  (let ((id 0)
        (serializer (make-serializer)))
    (serializer
      (lambda ()
        (set! id (+ id 1))
        id))))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
        (id (generate-id)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'id) id)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (define (do-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
      ((serializer2 (serializer1 exchange))
       account1
       account2)))
  (let ((id1 (account1 'id))
        (id2 (account2 'id)))
    (if (< id1 id2)
      (do-exchange account1 account2)
      (do-exchange account2 account1))))
