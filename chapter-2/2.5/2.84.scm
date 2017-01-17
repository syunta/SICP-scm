(load "./2.83")

; 強制型変換 t1 -> t2, t2 -> t1 をそれぞれ行い、
; 型が変わった方が塔の中で低いと判断できる。
;
; 試しに塔の構造に実数を追加しても、実数の直下の型の型変換定義を修正するだけで良い
;
; scheme-number -> rational -> complex
; scheme-number -> rational -> real-number -> complex

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
            (let ((new-a1 (raise type2 a1))
                  (new-a2 (raise type1 a2)))
              (cond ((eq? type2 (type-tag new-a1))
                      (apply-generic op new-a1 a2))
                    ((eq? type1 (type-tag new-a2))
                      (apply-generic op a1 new-a2))
                    (else
                      (error "No method for these types"
                             (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise 'rational
       (lambda (x) (make-real-number (inexact (/ (numer x) (denom x))))))
  'done)

(define (install-real-number-package)
  (define (tag x)
    (attach-tag 'real-number x))
  (put 'add '(real-number real-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real-number real-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real-number real-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real-number real-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real-number
       (lambda (x) (tag x)))
  (put 'raise 'real-number
       (lambda (x) (make-complex-from-real-imag x 0)))
  'done)

(define (make-real-number n)
  ((get 'make 'real-number) n))

(define (main args)
  (install-polar-package)
  (install-rectangular-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-real-number-package)
  (install-complex-package)
  (print (add (make-scheme-number 10)
              (make-complex-from-real-imag 10 5)))
  ;=> (complex rectangular 20 . 5)
  (print (add (make-complex-from-real-imag 5 3)
              (make-scheme-number 20)))
  ;=> (complex rectangular 25 . 3)
  (print (add (make-rational 1 4)
              (make-real-number 20)))
  ;=> (real-number 20 . 25)
  (print (add (make-rational 1 4)
              (make-scheme-number 1)))
  ;=> (ratinal 5 . 4)
  )
