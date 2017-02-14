(load "./2.95")

(define (install-polynomial-package)
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

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

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
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

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
      (let ((result (div-terms (term-list p1) (term-list p2))))
        (list (make-poly (variable p1) (car result))
              (make-poly (variable p1) (cadr result))))
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
              (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                    (cadr rest-of-result))))))))

  (define (pseudoremainder-terms P Q)
    (let ((O1 (order (first-term P)))
          (O2 (order (first-term Q)))
          (c (coeff (first-term Q))))
      (let ((integerizing-factor (expt c (+ 1 (- O1 O2)))))
        (cadr (div-terms (mul-term-by-all-terms (make-term 0 integerizing-factor) P)
                         Q)))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (gcd-coeff ts)
    (apply gcd (map coeff ts)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
      (mul-term-by-all-terms (make-term 0 (/ 1 (gcd-coeff a))) a)
      (gcd-terms b (pseudoremainder-terms a b))))

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
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (list (tag (car result))
                 (tag (cadr result))))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero-poly? p)))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (main args)
  (install-polar-package)
  (install-rectangular-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-polynomial-package)
  (let ((p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
        (p2 (make-polynomial 'x '((2 11) (0 7))))
        (p3 (make-polynomial 'x '((1 13) (0 5)))))
    (let ((q1 (mul p1 p2))
          (q2 (mul p1 p3)))
      (print (greatest-common-divisor q1 q2))))
  ; a
  ;=> (polynomial x (2 1458) (1 -2916) (0 1458))

  ; b
  ;=> (polynomial x (2 1) (1 -2) (0 1))
  )
