(load "./4.06")

;次のlet*式は、

;(let* ((var1 exp1)
;       (var1 exp1)
;       ...
;       (varn expn))
;  body)

(define let*-expression
  '(let* ((x 3)
          (y (+ x 2))
          (z (+ x y 5)))
     (* x z)))

;以下のlet式に書き直せる

;(let ((var1 exp1))
;  (let ((var2 exp2))
;    ...
;      (let ((varn expn))
;  body)

(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z))))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (first-binding bindings) (car bindings))
(define (rest-bindings bindings) (cdr bindings))

(define (let*->nested-lets exp)
  (define (go bindings)
    (if (null? bindings)
      (let-body exp)
      (let ((rest (rest-bindings bindings))
            (binding (list (first-binding bindings))))
        (if (null? rest)
          (make-let binding (go rest))
          (make-let binding (list (go rest)))))))
  (go (let-bindings exp)))

(define (main args)
  (print (let*->nested-lets let*-expression))
  ;=>
  ;(let ((x 3))
  ;  (let ((y (+ x 2)))
  ;    (let ((z (+ x y 5)))
  ;      (* x z))))
  )

;evalには節を追加するだけで十分である。
