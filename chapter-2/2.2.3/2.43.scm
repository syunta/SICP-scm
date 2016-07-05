(load "./2.41")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define empty-board nil)

(define (safe? k positions)
  (define (unique? x pos)
    (cond
      ((null? pos) #t)
      ((equal? (car pos) x) #f)
      (else (unique? x (cdr pos)))))
  (define (oblique? up down pos)
    (cond
      ((null? pos) #t)
      ((or (equal? up (car pos)) (equal? down (car pos))) #f)
      (else (oblique? (+ up 1) (- down 1) (cdr pos)))))
  (cond
    ((not (unique? (car positions) (cdr positions))) #f)
    ((not (oblique? (+ (car positions) 1) (- (car positions) 1) (cdr positions))) #f)
    (else #t)))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

; もともとのプログラムのqueen-colsの呼び出しが線形再帰であるのに対し、
; Rouisのプログラムではqueen-colsの呼び出しが木構造再帰になってしまっている。

; 例えば (queens 4) の場合

; もともとのプログラムでは以下の呼び出しであるのに対し、

; k=4: (queen-cols 3) が1回呼び出される
; k=3: (queen-cols 2) が1回呼び出される
; k=2: (queen-cols 2) が1回呼び出される
; k=1: (queen-cols 1) が1回呼び出される

; Rouisのプログラムでは以下の呼び出しとなる。

; k=4: (queen-cols 3) が4回呼び出される
; k=3: (queen-cols 2) が4回呼び出される
; k=2: (queen-cols 2) が4回呼び出される
; k=1: (queen-cols 1) が4回呼び出される

; (queens 4) の実行時間はほぼqueen-colsの呼び出し回数に依存しているので、T * 4 * 4 * 4 * 4 の時間がかかる。

; よって、 Tn^n と推定できる。

(define (main args)
  (print (queens 4))
  )
