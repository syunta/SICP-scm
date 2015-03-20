(load "../../lib/stream")

(define (stream-limit stream tolerance)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream))))
    (if (< (abs (- s1 s2)) tolerance)
      s2
      (stream-limit (stream-cdr stream) tolerance))))

(print (stream-limit (sqrt-stream 2) 0.0000001))
;=> 1.414213562373095
