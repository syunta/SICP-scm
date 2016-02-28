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

(define (merge-frame frame1 frame2)
  (merge-binding (append frame1 frame2) '()))

(define (merge-binding frame merged)
  (cond ((eq? merged 'failed) 'failed)
        ((null? frame) merged)
        (else
          (let ((binding (car frame))
                (rest-frame (cdr frame)))
            (merge-binding rest-frame
                           (merge-if-possible (binding-variable binding)
                                              (binding-value binding)
                                              rest-frame
                                              merged))))))

(define (merge-if-possible var val frame merged)
  (let ((binding (binding-in-frame var frame)))
    (cond ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
               (merge-if-possible
                 var (binding-value binding) frame merged)
               (extend var val merged))))
          (binding
            (if (equal? val (binding-value binding))
              (extend var val merged)
              'failed))
          ((depends-on? val var frame)
           'failed)
          (else (extend var val merged)))))

(define merge-frame-test-1
  '(((? x) . 1)
    ((? y) . 2)
    ((? z) . 3)
    ((? v) . (? w))
    ((? a) . (? b))))

(define merge-frame-test-2
  '(((? x) . 1)
    ;((? y) . 5)
    ((? u) . 4)
    ((? w) . 5)
    ((? v) . 5)))

(define (main args)
  (print (merge-frame merge-frame-test-1 merge-frame-test-2))
  ;=> (((? v) . 5) ((? w) . 5) ((? u) . 4) ((? x) . 1) ((? a) ? b) ((? v) . 5) ((? z) . 3) ((? y) . 2) ((? x) . 1))
  )
