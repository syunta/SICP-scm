(load "./2.85")
(load "./2.91")

(define (apply-generic op . args)
  (define (iter . args)
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
                (cond ((eq? type1 type2) a1)
                      ((eq? (type-tag new-a1) type2)
                        (apply-generic op new-a1 a2))
                      ((eq? type1 (type-tag new-a2))
                        (apply-generic op a1 new-a2))
                      (else
                        (error "No method for these types"
                               (list op type-tags))))))
            (error "No method for these types"
                   (list op type-tags)))))))
  (let ((val (apply iter args)))
    (if (pair? val)
      (drop val)
      val)))

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put '=zero? '(scheme-number) zero?)
  (put 'make 'scheme-number (lambda (x) x))
  (put 'raise 'scheme-number
       (lambda (x) (make-polynomial 'x `((0 ,x)))))
  'done)

;(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (make-termlist term)
    (adjoin-term term (the-empty-termlist)))

  (define (variable<? v1 v2)
    (let ((s1 (symbol->string v1))
          (s2 (symbol->string v2)))
      (string>? s1 s2)))

  (define (expand-poly p)
    (make-poly (variable p) (expand-terms (term-list p))))

  (define (expand-terms ts)
    (flatmap-terms expand-term ts))

  (define (expand-term term)
    (let ((c (coeff term)))
      (if (eq? 'polynomial (type-tag c))
        (let ((poly (expand-poly (contents c))))
          (make-term-by-all-coeffs (order term)
                                   (terms->coeffs (variable (contents c))
                                                  (term-list poly))))
        (adjoin-term term (the-empty-termlist)))))

  (define (make-term-by-all-coeffs order cs)
    (if (null? cs)
      (the-empty-termlist)
      (adjoin-term (make-term order (car cs))
                   (make-term-by-all-coeffs order (cdr cs)))))

  (define (terms->coeffs var ts)
    (if (empty-termlist? ts)
      '()
      (let ((t (first-term ts)))
        (if (zero? (order t))
          (cons (make-scheme-number (coeff t))
                (terms->coeffs var (rest-terms ts)))
          (cons (make-polynomial var `((,(order t) ,(coeff t))))
                (terms->coeffs var (rest-terms ts)))))))

  (define (scan-out var term)
    term)
  ;  (define (scan-out-vars t)
  ;    (let ((c (coeff t)))
  ;      (if (not (eq? 'polynomial (type-tag c)))
  ;        (scan-out-rest term)
  ;        (let ((poly (contents c)))
  ;          (if (eq? var (variable poly))
  ;            (scan-out-vars (+ (order t) (order scaned))
  ;                           (first-term (term-list poly))))
  ;            (scan-out-vars (first-term (term-list poly))))))))
  ;  (define (scan-out-rest t)
  ;    (let ((c (coeff t)))
  ;      (if (not (eq? 'polynomial (type-tag c)))
  ;        t
  ;        (let ((poly (contents c)))
  ;          (if (eq? var (variable poly))
  ;            (scan-out-rest (first-term (term-list poly)))
  ;            (make-polynomial var 
  ;                             (scan-out-rest (first-term (term-list poly)))))))))
  ;  (scan-out-vars term))

  (define (contains-variable? var term)
    (let ((c (coeff term)))
      (and (eq? 'polynomial (type-tag c))
           (or (eq? var (variable (contents c)))
               (contains-variable? var
                                   (first-term (term-list (contents c))))))))

  (define (const? p)
    (let ((ts (term-list p)))
      (or (empty-termlist? ts)
          (= (order (first-term ts)) 0))))

  (define (map-terms f ts)
    (if (empty-termlist? ts)
      (the-empty-termlist)
      (adjoin-term (f (first-term ts))
                   (map-terms f (rest-terms ts)))))

  (define (flatmap-terms f ts)
    (if (empty-termlist? ts)
      (the-empty-termlist)
      (append-terms (f (first-term ts))
                    (flatmap-terms f (rest-terms ts)))))

  (define (filter-terms pred ts)
    (cond ((empty-termlist? ts) (the-empty-termlist))
          ((pred (first-term ts))
           (adjoin-term (first-term ts)
                        (filter-terms pred (rest-terms ts))))
          (else (filter-terms pred (rest-terms ts)))))

  (define append-terms append)

  (define (convert-poly var p)
    (let* ((ts (term-list (expand-poly p)))
           (ts1 (filter-terms (lambda (t) (contains-variable? var t)) ts))
           (consts (filter-terms (lambda (t) (not (contains-variable? var t))) ts)))
      (make-poly
        var
        (append-terms
          (map-terms (lambda (t) (scan-out var t)) ts1)
          (adjoin-term (make-term 0 (make-polynomial (variable p) consts))
                       (the-empty-termlist))))))

  (define (add-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (cond ((same-variable? v1 v2)
             (make-poly v1 (add-terms (term-list p1) (term-list p2))))
            ((const? p1)
             (add-poly (make-poly v2 (term-list p1)) p2))
            ((const? p2)
             (add-poly p1 (make-poly v1 (term-list p2))))
            ((variable<? v1 v2)
             (add-poly (convert-poly v2 p1) p2))
            (else
              (add-poly p1 (convert-poly v1 p2))))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (cond ((same-variable? v1 v2)
             (make-poly v1 (mul-terms (term-list p1) (term-list p2))))
            ((const? p1)
             (mul-poly (make-poly v2 (term-list p1)) p2))
            ((const? p2)
             (mul-poly p1 (make-poly v1 (term-list p2))))
            ((variable<? v1 v2)
             (mul-poly (convert-poly v2 p1) p2))
            (else
              (mul-poly p1 (convert-poly v1 p2))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (- (order t1) (order t2))))
            (let ((rest-of-result
                    (div-terms
                      (sub-terms
                        L1
                        (mul-terms
                          (adjoin-term
                            (make-term new-o new-c)
                            (the-empty-termlist))
                          L2))
                      L2)))
              (adjoin-term (make-term new-o new-c) rest-of-result)))))))

  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))

  (define (negate-terms ts)
    (if (empty-termlist? ts)
      (the-empty-termlist)
      (let ((t1 (first-term ts)))
        (adjoin-term (make-term (order t1) (negate (coeff t1)))
                     (negate-terms (rest-terms ts))))))

  (define (=zero-poly? p)
    (=zero-terms? (term-list p)))

  (define (=zero-terms? ts)
    (if (empty-termlist? ts)
      #t
      (and (=zero? (coeff (first-term ts)))
           (=zero-terms? (rest-terms ts)))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero-poly? p)))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
;  'done)

; 1 + 1
;(polynomial x (0 1))

; x + y
;(polynomial x (0 (polynomial y (1 1))))
;
; (x^2 + 1)y
; (x^2)y + (1)y
; (y)x^2 + (y)x^0

(define (main args)
  (install-polar-package)
  (install-rectangular-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  ;(install-polynomial-package)

  ;(print (add (make-polynomial 'x '((1 1) (0 9)))
  ;            (make-polynomial 'y '((1 1) (0 10)))))
  ;(print (add 10 (make-polynomial 'y '((1 1) (0 10)))))

  ;(print (mul (make-polynomial 'y '((1 1)))
  ;            (make-polynomial 'x '((1 1)))))
  ;(print (add '(polynomial x (1 1))
  ;            '(polynomial y (1 (polynomial y (2 1) (1 10))))))
 
  (print (convert-poly 'x '(y (1 (polynomial x (2 1) (1 10))))))

  ; (y^2 + 10y + 10)x
  ;(print (expand-poly '(y (2 (polynomial x (1 1) (0 3))) (1 10) (0 10))))

  ; ((x + 3)y^2 + 10y + 10)x
  ;=> ((x)y^2)x + 3y^2x + 10yx + 10x
  ;(print (expand-poly '(x (1 (polynomial y (2 (polynomial x (1 1) (0 3))) (1 10) (0 10))))))
  (print (scan-out 'x '(1 (polynomial y (2 (polynomial x (1 1)))))))
  )
