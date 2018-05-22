(load "./eval-apply")

;frame construction: ((var1 . val1) (var2 . val2) ... )

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame) (map car frame))

(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

;このままだと新しい束縛を定義することはできるが、上書きすることはできない。
;define-variable!, set-variable-value! の (set-car! vals val) で参照されるvalsは、mapで作られた別オブジェクトであるため。
;結局、define-variable!などにも修正が必要。

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
    (error "Unbound variable -- SET!" var)
    (let* ((frame (first-frame env))
           (bind (assoc var frame)))
      (if bind
        (set-cdr! bind val)
        (set-variable-value! var val (enclosing-environment env))))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (bind (assoc var frame)))
    (if bind
      (set-cdr! bind val)
      (add-binding-to-frame! var val frame))))
