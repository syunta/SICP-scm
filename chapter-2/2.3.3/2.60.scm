(load "./2.59")

; O(n) -> O(n)
; element-of-set? は変更不要

; O(n) -> O(1)
(define (adjoin-set x set)
  (cons x set))

; O(n^2) -> O(1)
(define (union-set set1 set2)
  (append set1 set2))

; O(n^2) -> O(n^2 + m^2)
(define (intersection-set set1 set2)
  (define (go set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1) (go (cdr set1) set2)))
          (else (go (cdr set1) set2))))
  (append (go set1 set2)
          (go set2 set1)))

(define (main args)
  (print (union-set '(2 3 2 1 3 2 2) '(4 4 1 4 3)))
  ;=> (2 3 2 1 3 2 2 4 4 1 4 3)
  (print (intersection-set '(2 3 2 1 3 2 2) '(4 4 1 4 3)))
  ;=> (3 1 3 1 3)

  ; union-setは効率が良いので、頻繁にunion-setを使う必要がある場合に重複あり表現を使いたくなる。
  )
