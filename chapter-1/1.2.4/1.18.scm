(load "./1.17")

(define (mul a b)
  (mul-iter a b 0))

(define (mul-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (mul-iter (double a) (halve b) p))
        (else (mul-iter a (- b 1) (+ p a)))))

; ロシア農民の方法
;
;   45 * 113
;
;   45   113   +45
;   90    56
;  180    28
;  360    14
;  720     7   +720
; 1440     3   +1440
; 2880     1   +2880
;
; = 5085

(define (halve-floor x)
  (truncate (/ x 2)))

(define (russian-peasant-mul a b)
  (mul-iter a b 0))

(define (r-mul-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (r-mul-iter (double a) (halve-floor b) p))
        (else (r-mul-iter (double a) (halve-floor b) (+ p a)))))

(define (main args)
  (print (mul 3 1))
  (print (mul 3 2))
  (print (mul 3 3))
  (print (mul 3 4))
  (print (russian-peasant-mul 45 113))
  )
