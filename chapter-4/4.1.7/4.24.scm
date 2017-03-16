(load "../../lib/separated-analysis-excution-eval-apply")
;(load "../../lib/eval-apply")

(define (runtime)
  (round->exact (* (expt 10 6)
                   (time->seconds (current-time)))))

(define (time-eval exps env)
  (let ((start (runtime)))
    (print "START -- " start)
    (print (eval exps env))
    (let ((end (runtime)))
      (print "END -- " end)
      (print "DIFF -- " (- end start)))))

(define (make-begin-exp count)
  (define (iter n)
    (if (= n 0)
      '()
      (cons 1
            (iter (- n 1)))))
  (cons 'begin (iter count)))

(define (main args)
  (time-eval
    '(begin
       (define (fib n)
         (cond ((= n 0) 0)
               ((= n 1) 1)
               (else (+ (fib (- n 1))
                        (fib (- n 2))))))
       (fib 20))
    the-global-environment)
  ; 始めの超循環評価器
  ; START -- 1489667774188367
  ; 6765
  ; END -- 1489667774473911
  ; DIFF -- 285544

  ; 本節での版
  ; START -- 1489666820916571
  ; 6765
  ; END -- 1489666821063908
  ; DIFF -- 147337


  ; (begin <exp1> <exp2> ... )
  ; のように、手続き呼び出しが無く、解析が再利用できないパターンでは遅い
  (time-eval
    (make-begin-exp 1000000)
    the-global-environment)
  ; 始めの超循環評価器
  ; DIFF -- 146542

  ; 本節での版
  ; DIFF -- 1555061
  (time-eval
    (make-begin-exp 10000000)
    the-global-environment)
  ; 始めの超循環評価器
  ; DIFF -- 1259311

  ; 本節での版
  ; DIFF -- 12104385

  ; (begin <exp1> <exp2> ... )
  ; 上記のコードでは、始めの超循環評価器では、全体の実行時間のうち、実行に使われる時間がほとんどを占める
  ; 本節での版では、全体の実行時間は、解析と実行に使われる時間の割合で構成される
  ; 始めの超循環評価器での時間(ほぼ実行時間)と本節での版の時間比は、約 1:10
  ; よって、解析と実行に使われる時間の割合は、9:1 と考えられる。
  )
