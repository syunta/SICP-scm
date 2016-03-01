(add-load-path "../../lib" :relative)
(load "query")

; frame1 frame2
; (x 1)  (x 1) => success (x 1)
; (y 2)  (y 5) => failed
; (z 3)        => success (z 3)
;        (u 4) => success (u 4)
; (v w)  (v 5) => success (v 5) (w 5)
; (v w)  (w 5) => success (v 5) (w 5)
; (a b)        => success (a b)

; frameの変数には、変数か値しか束縛されないと思われるので depends-on? などのチェックは不要

(define (merge-frame frame merged)
  (cond ((eq? merged 'failed) 'failed)
        ((null? frame) merged)
        (else
          (let ((binding (car frame))
                (rest-frame (cdr frame)))
            (merge-frame rest-frame
                         (merge-match (binding-variable binding)
                                      (binding-value binding)
                                      merged))))))

(define (merge-match v1 v2 merged)
  (cond ((eq? merged 'failed) 'failed)
        ((equal? v1 v2) merged)
        ((var? v1) (merge-if-possible v1 v2 merged))
        ((var? v2) (merge-if-possible v2 v1 merged))
        (else 'failed)))

(define (merge-if-possible var val merged)
  (let ((binding (binding-in-frame var merged)))
    (cond (binding
            (merge-match
              val (binding-value binding) merged))
          ((var? val)
           (let ((binding (binding-in-frame val merged)))
             (if binding
               (merge-match
                 var (binding-value binding) merged)
               (extend var val merged))))
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
  ;=> (((? a) ? b) ((? x) . 1) ((? u) . 4) ((? w) . 5) ((? v) . 5))
  (print-qeval '(wheel ?x))
  ;=>
  ; (wheel (Warbucks Oliver))
  ; (wheel (Warbucks Oliver))
  ; (wheel (Bitdiddle Ben))
  ; (wheel (Warbucks Oliver))
  ; (wheel (Warbucks Oliver))
  (print-qeval '(lives-near ?x ?y))
  ; この実装では、andの各節を別々に処理する。
  ; このため、lives-near のnot節が束縛が作られていない状態で評価されてしまい結果のフレームを生じない。

  ;(print-qeval '(outranked-by ?x ?y))
  ; outranked-by は規則を再帰的に呼び出している。
  ; このため、束縛が作られていない状態で outranked-by が呼ばれ続けることになり、無限ループに陥る。
  )
