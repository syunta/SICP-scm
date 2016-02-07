(add-load-path "../../lib" :relative)
(load "query")

; ある規則に対して質問が適用される時、質問が同じである限りユニファイの結果も常に同じだと考えられる。
; つまり、すでに評価された規則に対して同じ質問が適用される場合、フレームを拡張する必要はない。

; このことから、無限ループ検出器を次のように実装することができる。

; 推論の鎖には、評価された規則と質問の組を保持する
; ある規則の評価中、すでに同じ質問でユニファイされていた場合、空フレームを返す
; ある規則の評価が終わったら推論の鎖をリセットする

(define CHAIN '())

(define (remove-application-id-in query)
  (cond ((and (var? query)
              (number? (cadr query)))
         (cons (car query) (cddr query)))
        ((pair? query)
         (cons (remove-application-id-in (car query))
               (remove-application-id-in (cdr query))))
        (else query)))

(define (applied? rule query)
  (let ((key (cons rule (remove-application-id-in query))))
    (assoc key CHAIN)))

(define (add-chain! rule query)
  (let ((key (cons rule (remove-application-id-in query))))
    (set! CHAIN (cons (cons key 'applied) CHAIN))))

(define (reset-chain!)
  (set! CHAIN '()))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (let ((result (apply-a-rule rule pattern frame)))
                      (reset-chain!)
                      result))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (cond ((applied? rule query-pattern) the-empty-stream)
        (else
          (add-chain! rule query-pattern)
          (let ((clean-rule (rename-variables-in rule)))
            (let ((unify-result
                    (unify-match query-pattern
                                 (conclusion clean-rule)
                                 query-frame)))
              (if (eq? unify-result 'failed)
                the-empty-stream
                (qeval (rule-body clean-rule)
                       (singleton-stream unify-result))))))))

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (married Minnie Mickey)
            (rule (married ?x ?y)
                  (married ?y ?x))

            (rule (outranked-by-Rouis-version ?staff-person ?boss)
                  (or (supervisor ?staff-person ?boss)
                      (and (outranked-by ?middle-manager ?boss)
                           (supervisor ?staff-person ?middle-manager))))
            ))

(define (main args)
  (print (remove-application-id-in '(married (? 1 y) (? 1 x))))
  ;=> (married (? y) (? x))
  (print (remove-application-id-in '(married (? y) (? x))))
  ;=> (married (? y) (? x))

  (print-qeval '(married Mickey ?who))
  ;=> (married Mickey Minnie)
  (print-qeval '(outranked-by-Rouis-version (Bitdiddle Ben) ?who))
  ;=> (outranked-by-Rouis-version (Bitdiddle Ben) (Warbucks Oliver))
  )
