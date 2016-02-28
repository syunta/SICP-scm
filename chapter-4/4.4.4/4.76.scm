(add-load-path "../../lib" :relative)
(load "query")

; frame1 frame2
; (x 1)  (x 1) => success
; (y 2)  (y 5) => failed
; (z 3)        => success
;        (u 4) => success
; (v w)  (w 5)
;        (v 5) => success
; (a b)        => success
; (c (c 6))    => failed
