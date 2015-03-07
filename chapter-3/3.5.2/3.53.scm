(load "../../lib/stream")

(define s (cons-stream 1 (add-streams s s)))

; sの要素
; 1 2 3 8 16 32 64 128 256 512 1024 2048 4096 ...

(display-stream (stream-take s 12))
