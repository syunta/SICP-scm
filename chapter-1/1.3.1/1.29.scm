(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; y0 + 4y1 + 2y2 + 4y3 + 2y4 + ...
; y0 + 4y1 + y2 + y2 + 4y3 + y4 + y4 + ...
; (y0 + 4y1 + y2) + (y2 + 4y3 + y4) + ...

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (let ((inc-2 (lambda (x)
                   (+ x 2)))
          (f2 (lambda (k)
                (+ (f (+ a (* k h)))
                   (* 4 (f (+ a (* (+ 1 k) h))))
                   (f (+ a (* (+ 2 k) h)))))))
      (* (/ h 3.0)
         (sum f2 0 inc-2 n)))))

(define (main args)
  (print (integral cube 0 1 0.01))
  ;=> 0.24998750000000042
  (print (simpson-integral cube 0 1 100))
  ;=> 0.27060804
  (print (simpson-integral cube 0 1 1000))
  ;=> 0.25200600800399997
  )
