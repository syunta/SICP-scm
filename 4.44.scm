(load "./amb")

(for-each simple-ambeval
          '((define (queens board-size)
              (define (safe? positions)
                (define (unique? x pos)
                  (cond ((null? pos) true)
                        ((= (car pos) x) false)
                        (else (unique? x (cdr pos)))))
                (define (oblique? up down pos)
                  (cond ((null? pos) true)
                        ((= up (car pos)) false)
                        ((= down (car pos)) false)
                        (else (oblique? (+ up 1) (- down 1) (cdr pos)))))
                (cond ((not (unique? (car positions) (cdr positions))) false)
                      ((not (oblique? (+ (car positions) 1) (- (car positions) 1) (cdr positions))) false)
                      (else true)))
              (define (go n cols)
                (if (= n 0)
                  cols
                  (let ((col (an-element-of (iota board-size 1 1))))
                    (let ((new-cols (cons col cols)))
                      (require (safe? new-cols))
                      (go (- n 1) new-cols)))))
              (let ((answer (go board-size '())))
                answer))
            ))

; (3 1 4 2)
; リストの要素数がクイーンの横位置を表す
; リストの要素の数字がクイーンの縦位置を表す

(define (main args)
  (print-ambeval '(queens 8) 5)
  ;=>
  ; (4 2 7 3 6 8 5 1)
  ; (5 2 4 7 3 8 6 1)
  ; (3 5 2 8 6 4 7 1)
  ; (3 6 4 2 8 5 7 1)
  ; (5 7 1 3 8 6 4 2)
  ; To be continued ...
  )
