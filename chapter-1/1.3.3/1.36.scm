(define tolerance 0.00001)
(define count 0)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (set! count (+ count 1))
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (f1 x)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))

(define (average x y)
  (/ (+ x y) 2))

(define (f2 x)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               2.0))

(define (main args)
  (f1 1000)
  (print count)
  ;=> 34
  (set! count 0)
  (f2 1000)
  (print count)
  ; 9
  ; 平均緩和を使った方が早く収束する
  )
