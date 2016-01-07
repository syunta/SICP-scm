(add-load-path "../../lib" :relative)
(load "./4.51")
(load "./4.52")

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((amb? exp) (analyze-amb exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(for-each simple-ambeval
          '(
            (define (square x)
              (* x x))

            (define (smallest-divisor n)
              (find-divisor n 2))

            (define (find-divisor n test-divisor)
              (cond ((> (square test-divisor) n) n)
                    ((divides? test-divisor n) test-divisor)
                    (else (find-divisor n (+ test-divisor 1)))))

            (define (divides? a b)
              (= (remainder b a) 0))

            (define (prime? n)
              (= n (smallest-divisor n)))

            (define (prime-sum-pair list1 list2)
              (let ((a (an-element-of list1))
                    (b (an-element-of list2)))
                (require (prime? (+ a b)))
                (list a b)))
            ))

(define (main args)
  (print-ambeval '(prime? 43) 1)
  (print-ambeval '(prime? 113) 1)
  (print-ambeval '(prime? 23) 1)

  (print-ambeval
    '(let ((pairs '()))
       (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
                  (permanent-set! pairs (cons p pairs))
                  (amb))
                pairs))
    1)
  ;=> ((8 35) (3 110) (3 20))
  ;        43     113     23 
  ; prime-sum-pairs が成功した組み合わせのリストが返ってくる。
  )
