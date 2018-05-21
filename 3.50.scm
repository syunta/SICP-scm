(load "./stream")

(display-stream
  (stream-map (lambda (x y) (* x y))
              (stream 1 2 3 4)
              (stream 2 2 3 4 5)))
;=>

;2
;4
;9
;16

(display-stream
  (stream-map (lambda (x y) (* x y))
              (stream 1 2 3 4 5)
              (stream 1 2 3)))
;=>

;1
;4
;9
