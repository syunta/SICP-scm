(load "../../lib/stream")

(define s (cons-stream 1
                       (merge (scale-stream s 5)
                              (merge (scale-stream s 3)
                                     (scale-stream s 2)))))

(display-stream (stream-take s 15))
;=>
;1
;2
;3
;4
;5
;6
;8
;9
;10
;12
;15
;16
;18
;20
;24
;25
