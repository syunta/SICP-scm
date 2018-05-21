(load "./stream")

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)

(display-stream z)

;各式が評価し終わった時のsum: 210

;stream-refとdisplay-streamの評価に応じて印字される応答

;(stream-ref y 7)
;=> nothing
;(display-stream z)
;=>
;10
;15
;45
;55
;105
;120
;190
;210


;memo-procの用意する最適化を使わなかった時

;(display-stream z)の印字する結果が違う
;15
;180
;230
;305
