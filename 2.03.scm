(load "./2.02")

; A            B
;  *----------*
;  |          |
;  |          |
;  *----------*
; D            C

(define (make-rectangle a b c d)
  (cons a (cons b (cons c d))))

(define (a-point r) (car r))
(define (b-point r) (cadr r))
(define (c-point r) (caddr r))
(define (d-point r) (cdddr r))

(define (width r)
  (abs (- (x-point (a-point r))
          (x-point (b-point r)))))

(define (height r)
  (abs (- (y-point (a-point r))
          (y-point (c-point r)))))

(define (perimeter r)
  (* 2 (+ (width r) (height r))))

(define (area r)
  (* (width r) (height r)))

(define test-rectangle
  (make-rectangle (make-point 0 10)
                  (make-point 15 10)
                  (make-point 15 0)
                  (make-point 0 0)))

(define (make-rectangle-2 a c)
  (cons a c))

(define (a-point-2 r) (car r))
(define (c-point-2 r) (cdr r))

(define (width-2 r)
  (abs (- (x-point (a-point-2 r))
          (x-point (c-point-2 r)))))

(define (height-2 r)
  (abs (- (y-point (a-point-2 r))
          (y-point (c-point-2 r)))))

(define test-rectangle-2
  (make-rectangle-2 (make-point 0 10)
                    (make-point 15 0)))

(define (main args)
  (print (perimeter test-rectangle))
  (print (area test-rectangle))

  (set! height height-2)
  (set! width width-2)

  (print (perimeter test-rectangle-2))
  (print (area test-rectangle-2))

  ; 長方形を扱う構成子,選択子は変わったが perimeter, area 手続きは同じものを使っている。
  ; 長方形を扱う構成子,選択子の上に長方形計算手続きが実装されているので良好な抽象の壁で設計されている。
  )
