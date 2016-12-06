(load "./2.73")

(define (deriv ex var)
  ((get 'deriv (operator ex)) (operands ex) var))

(define (install-number-package)
  (define (num operands var) 0)
  (put 'deriv 'num num)
  'done)

(define (install-variable-package)
  (define (var operands var)
    (let ((exp (car operands)))
      (if (same-variable? exp var) 1 0)))
  (put 'deriv 'var var)
  'done)

(define (print-deriv seq)
  (let ((ex (car seq)) (var (cadr seq)))
    (print (deriv ex var))))

(define (main args)
  (install-number-package)
  (install-variable-package)
  (install-sum-package)
  (install-product-package)
  (install-exponentiation-package)
  (map print-deriv
       (list '((* (var x) (num 4)) x)
             '((* (var x) (var y)) x)
             '((+ (var x) (num 3)) x)
             '((* (* (var x) (var y)) (+ (var x) (num 3))) x)))
  )
; こんな感じで変数や数値もリストにしてタグを付ければテーブルに吸収できる。
; 問題で吸収できないと言っている理由は、変数や数値の形式を変えてはならないから
