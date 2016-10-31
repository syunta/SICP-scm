(load "./2.57")

(define (sum? e)
  (and (pair? e) (memq '+ e)))

(define (addend e)
  (define (iter e result)
    (cond ((and (eq? (car e) '+) (null? (cdr result)))
           (car result))
          ((eq? (car e) '+)
           (reverse result))
          (else (iter (cdr e)
                      (cons (car e) result)))))
  (iter e '()))

(define (augend e)
  (cond ((and (eq? (car e) '+) (null? (cddr e)))
         (cadr e))
        ((eq? (car e) '+)
         (cdr e))
        (else (augend (cdr e)))))

(define (product? e)
  (and (pair? e) (memq '* e)))

(define (multiplier e) (car e))

(define (multiplicand e)
  (if (null? (cdddr e))
    (caddr e)
    (cddr e)))

(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else (list x '+ y))))

(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list x '* y))))

(define (exponentiation? e)
  (and (pair? e) (eq? (cadr e) '**)))

(define (base e) (car e))
(define (exponent e) (caddr e))

(define (make-exponentiation x y)
  (cond ((=number? y 0) 1)
        ((=number? y 1) x)
        (else (list x '** y))))

(define (main args)
  (print (deriv '(x + (3 * (x + (y + 2)))) 'x)) ;=> x + 3x + 3y + 6
  ;=> 4
  (print (deriv '(x + 3 * (x + y + 2)) 'x))
  ;=> 4
  ; できる。
  )
