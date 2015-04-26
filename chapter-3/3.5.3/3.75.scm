(load "./3.74")

; Louisの平滑化の誤りは、変換後の値を使って連続的に変換を行っていること。
; 直前の値との平滑化には、元の信号を使わなければならない。

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (average (stream-car input-stream) last-value)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(define zero-crossings
  (make-zero-crossings sense-data
                       0
                       (average 0
                                (stream-car sense-data))))

(define (main args)
  (display-stream
    (stream-take zero-crossings 11))
  ;=> 0 0 0 0 0 0 1 0 0 0 0 -1
  )
