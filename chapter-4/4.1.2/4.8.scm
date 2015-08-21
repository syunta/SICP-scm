(load "./4.7")

;以下の名前付きlet式は、

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))))

;次のように変換できる

(define (fib n)
  (let ((fib-iter 'false))
    (set! fib-iter (lambda (a b count)
                     (if (= count 0)
                       b
                       (fib-iter (+ a b) a (- count 1)))))
    (fib-iter 1 0 n)))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let-variables bindings)
  (map car bindings))

(define (let-expressions bindings)
  (map cadr bindings))

(define (named-let? exp)
  (symbol? (cadr exp)))

(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

(define (make-assignment name new-value)
  (list 'set! name new-value))

(define (let->combination exp)
  (cond ((named-let? exp)
         (let ((name (named-let-name exp))
               (bindings (named-let-bindings exp)))
           (make-let (list (list name 'false))
                     (list (make-assignment name
                                            (make-lambda (let-variables bindings)
                                                         (named-let-body exp)))
                           (cons name (let-expressions bindings))))))
        (else
          (let ((bindings (let-bindings exp)))
            (cons (make-lambda (let-variables bindings)
                               (let-body exp))
                  (let-expressions bindings))))))

(define (main args)

  (define named-let-expression
    '(let fib-iter ((a 1)
                    (b 0)
                    (count n))
       (if (= count 0)
         b
         (fib-iter (+ a b) a (- count 1)))))

  (print (let->combination named-let-expression))
  ;=>
  ;(let ((fib-iter false))
  ;  (set! fib-iter (lambda (a b count)
  ;                   (if (= count 0)
  ;                     b
  ;                     (fib-iter (+ a b) a (- count 1)))))
  ;  (fib-iter 1 0 n))
  )
