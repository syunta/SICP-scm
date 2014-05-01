(add-load-path "../.." :relative)
(load "lib/library.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
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
    ; positionsのcdr部分はsafeであることが保証されてるので、
    ; 調べるのはリストの先頭だけでよい（はず）
    ((not (unique? (car positions) (cdr positions))) #f)
    ((not (oblique? (+ (car positions) 1) (- (car positions) 1) (cdr positions))) #f)
    (else #t)))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

; (3 1 4 2)
; リストの要素がクイーンの横位置を表してる
; リストの要素の数字がクイーンの縦位置を表してる

; k なんていらんかったんや...

; 対称性も謎

(print (queens 5))
