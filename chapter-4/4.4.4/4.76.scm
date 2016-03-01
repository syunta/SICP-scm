(add-load-path "../../lib" :relative)
(load "query")

; frame1 frame2
; (x 1)  (x 1) => success (x 1)
; (y 2)  (y 5) => failed
; (z 3)        => success (z 3)
;        (u 4) => success (u 4)
; (v w)  (v 5) => success (v w) (v 5) (w 5)
; (v w)  (w 5) => success (v w) (v 5) (w 5)
; (a b)        => success (a b)
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
                           (merge-match (binding-variable binding)
                                        (binding-value binding)
                                        rest-frame
                                        merged))))))

(define (merge-match v1 v2 frame merged)
  (cond ((eq? merged 'failed) 'failed)
        ((equal? v1 v2) merged)
        ((var? v1) (merge-if-possible v1 v2 frame merged))
        ((var? v2) (merge-if-possible v2 v1 frame merged))
        (else 'failed)))

(define (merge-if-possible var val frame merged)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
            (merge-match
              val (binding-value binding) frame (extend var val merged)))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
               (merge-match
                 var (binding-value binding) frame (extend var val merged))
               (extend var val merged))))
          ((depends-on? val var frame)
           'failed)
          (else (extend var val merged)))))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (merge-conjunctions (rest-conjuncts conjuncts)
                        (qeval (first-conjunct conjuncts) frame-stream)
                        frame-stream)))

(define (merge-conjunctions conjuncts merged-frame-stream frame-stream)
  (if (empty-conjunction? conjuncts)
    merged-frame-stream
    (let ((qeval-result
            (qeval (first-conjunct conjuncts) frame-stream)))
      (let ((merge-result
              (stream-flatmap
                (lambda (frame1)
                  (stream-filter
                    (lambda (f) (not (eq? 'failed f)))
                    (stream-map
                      (lambda (frame2)
                        (merge-frame frame1 frame2))
                      qeval-result)))
                merged-frame-stream)))
        (merge-conjunctions (rest-conjuncts conjuncts)
                            merge-result
                            frame-stream)))))

(put 'and 'qeval conjoin)

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

(define test-query
  '(and (job ?person ?j)
        (address ?person ?where)
        (salary ?person ?amount)))

(define test-query
  '(lives-near ?x ?y))

(define (main args)
  (print (merge-frame merge-frame-test-1 merge-frame-test-2))
  ;=> (((? v) . 5) ((? w) . 5) ((? u) . 4) ((? x) . 1) ((? a) ? b) ((? w) . 5) ((? v) ? w) ((? z) . 3) ((? y) . 2) ((? x) . 1))
  (print-qeval test-query)
  ; TODO
  )
