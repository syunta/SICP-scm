(add-load-path "../2.5.1" :relative)
(load "2.77")

; a
; 無限のループに陥る

; b
; Louis は正しくない.
; このままでも型の演算が見つからない場合は, no method error となるので, 正しく働いていると言える.

; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (not (eq? type1 type2)) ; added
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))
                      (else
                        (error "No method for these types"
                               (list op type-tags)))))
              (error "No method for these types"
                     (list op type-tags))))
            (error "No method for these types"
                   (list op type-tags)))))))

(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (scheme-number->scheme-number n) n) ; Louis
  (define (complex->complex z) z) ; Louis
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number) ; Louis
  (put-coercion 'complex 'complex complex->complex) ; Louis
  'done)

(define (exp x y) (apply-generic 'exp x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (main args)
  (install-polar-package)
  (install-rectangular-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-coercion-package)
  ; test
  (print (add (make-scheme-number 10)
              (make-complex-from-real-imag 2 5)))
  ;=> (complex rectangular 12 . 5)
  ;(print (exp (make-complex-from-real-imag 10 0)
  ;            (make-complex-from-real-imag 2 0)))
  ;=> error
  )
