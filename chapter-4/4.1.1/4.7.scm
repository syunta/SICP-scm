;次のlet*式は、

;(let* ((var1 exp1)
;       (var1 exp1)
;       ...
;       (varn expn))
;  body)

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

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
