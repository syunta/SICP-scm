(load "./constraint_system.scm")

(define (squarere a b)
  (multiplier a a b))

(define a (make-connector))
(define b (make-connector))

(squarere a b)

(probe "a" a)
(probe "b" b)

(set-value! b 9 'user)

; b = a^2 の式は、bが決まればaも決まらなければならない。
; しかし、maltiplierは、2つ以上のコネクタに値が設定されていないと働かない。
; bだけが決まっても残り2つのコネクタには値が未設定なので、aが決まらないという欠点がある。
