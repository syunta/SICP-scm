(load "./2.56")

(define (augend s)
  (if (null? (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(define (multiplicand e)
  (if (null? (cdddr e))
    (caddr e)
    (cons '* (cddr e))))

(define (main args)
  (print (deriv '(+ x (+ x x) x x) 'x))
  ;=> 5
  (print (deriv '(* x y (+ x 3)) 'x)) ;=> x^2y + 3xy
  ;=> (+ (* x y) (* y (+ x 3))) => xy + xy + 3y => 2xy + 3y
  )
