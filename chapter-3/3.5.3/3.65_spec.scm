(load "./3.65")

; Verify ln2-summands
(newline)
(display-stream
  (stream-take (ln2-summands 1) 5))
;=>
;1.0
;-0.5
;0.3333333333333333
;-0.25
;0.2
;-0.16666666666666666

; plain version
(newline)
(display-stream
  (stream-take ln2-stream 5))
;=>
;1.0
;0.5
;0.8333333333333333
;0.5833333333333333
;0.7833333333333332
;0.6166666666666666

; euler-transform version
(newline)
(display-stream
  (stream-take (euler-transform ln2-stream) 5))
;=>
;0.7
;0.6904761904761905
;0.6944444444444444
;0.6924242424242424
;0.6935897435897436
;0.6928571428571428

; accelerated-sequence version
(newline)
(display-stream
  (stream-take (accelerated-sequence euler-transform ln2-stream) 5))
;=>
;1.0
;0.7
;0.6932773109243697
;0.6931488693329254
;0.6931471960735491
;0.6931471806635636
