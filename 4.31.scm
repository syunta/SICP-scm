(load "./normal-order-eval-apply")

; Usage
; (define (f a (lazy b) c (lazy-memo d)) ... )

(define (tagged-procedure-parameters p) (cadr p))
(define (procedure-parameters p)
  (map (lambda (param) (if (pair? param) (cadr param) param))
       (cadr p)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments
                                   (tagged-procedure-parameters procedure)
                                   env)
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

(define (first-parameter params) (car params))
(define (rest-parameters params) (cdr params))

(define (list-of-delayed-args exps parameters env)
  (if (no-operands? exps)
    '()
    (let ((param (first-parameter parameters))
          (operand (first-operand exps)))
      (let ((arg (cond ((tagged-list? param 'lazy) (delay-it-not-memo operand env))
                       ((tagged-list? param 'lazy-memo) (delay-it operand env))
                       ((not (pair? param)) (actual-value operand env))
                       (else (error "Unknown parameter tag" (car param))))))
        (cons arg (list-of-delayed-args (rest-operands exps)
                                        (rest-parameters parameters)
                                        env))))))

(define (delay-it-not-memo exp env)
  (list 'thunk-not-memo exp env))

(define (thunk-not-memo? obj)
  (tagged-list? obj 'thunk-not-memo))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        ((thunk-not-memo? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))

(define (main args)
  (actual-value '(define count 0)
                the-global-environment)

  (actual-value '(define (id x)
                   (set! count (+ count 1))
                   x)
                the-global-environment)

  ; lazy-test
  (actual-value '(define (square (lazy x))
                   (* x x))
                the-global-environment)

  (actual-value '(square (id 10))
                the-global-environment)
  (print (actual-value 'count the-global-environment))
  ;=> 2

  ; lazy-memo-test
  (actual-value '(define (square (lazy-memo x))
                   (* x x))
                the-global-environment)

  (actual-value '(square (id 10))
                the-global-environment)
  (print (actual-value 'count the-global-environment))
  ;=> 3

  ; no-tag-test
  (actual-value '(define (try x) 10)
                the-global-environment)
  (actual-value '(try (id 10))
                the-global-environment)

  (print (actual-value 'count the-global-environment))
  ;=> 4
  )
