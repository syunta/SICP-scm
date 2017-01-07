(add-load-path "../2.5.1" :relative)
(load "2.77")

; ex1
; (scheme-number rationl complex) : 初期状態
; (scheme-number rationl complex) : 第1引数のscheme-numberへ型変換
; (rational rationl complex) : 第2引数のratialへ型変換
; (complex comlex complex) : 第3引数のcomplexへ型変換
;
; ex2
; (ratial scheme-number complex) : 初期状態
; (ratial ratial complex) : 第1引数のrationalへ型変換
; (rational rational complex) : 第2引数のrationalへ型変換
; (complex comlex complex) : 第3引数のcomplexへ型変換
;
; 各引数の型変換の度、apply-genericを実行する
; 最後まで型変換した結果、演算が見つからない場合はエラー

(define (apply-generic op . args)
  (define (iter args rest-args)
    (let ((types (map type-tag args))
          (rest-types (map type-tag rest-args)))
      (let ((type-tags (append types rest-types)))
        (let ((proc (get op type-tags)))
          (if proc
            (apply proc (map contents (append args rest-args)))
            (if (null? rest-args)
              (error "No method for these types" (list op type-tags))
              (let ((type (car rest-types)))
                (let ((new-args (unify-types type args))
                      (new-rest-args (unify-types type rest-args)))
                  (iter (cons (car new-rest-args) new-args)
                        (cdr new-rest-args))))))))))
  (iter '() args))

(define (unify-types type1 args)
  (define (coerce a)
    (let ((type2 (type-tag a)))
      (let ((t2->t1 (get-coercion type2 type1)))
        (if t2->t1 (t2->t1 a) a))))
  (map coerce args))

(define (add . args)
  (apply apply-generic (cons 'add args)))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'add '(scheme-number scheme-number scheme-number)
       (lambda (x y z) (tag (+ x y z))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'add '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex z1 (add-complex z2 z3)))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

(define (main args)
  (install-polar-package)
  (install-rectangular-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-coercion-package)
  (print (add (make-scheme-number 10)
              (make-scheme-number 10)
              (make-scheme-number 10)))
  ;=> (scheme-number . 30)
  (print (add (make-complex-from-real-imag 10 2)
              (make-complex-from-real-imag 10 2)
              (make-complex-from-real-imag 10 2)))
  ;=> (complex rectangular 30 . 6)
  (print (add (make-scheme-number 10)
              (make-scheme-number 10)
              (make-complex-from-real-imag 10 2)))
  ;=> (complex rectangular 30 . 2)
  )
