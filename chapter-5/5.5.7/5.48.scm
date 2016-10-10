(add-load-path "../../lib" :relative)
(load "../../lib/load-eceval-compiler")
(load "../../lib/compiler")

(define (compile-and-run expression)
  (define (execute)
    (let ((insts (get-register-contents eceval 'pc)))
      (if (null? insts)
        'done
        (begin
          ((instruction-execution-proc (car insts)))
          (execute)))))
  (let ((instructions
          (assemble (statements
                      (compile expression 'val 'next)) ; returnではなくnextでコンパイルすることに注意する
                    eceval)))
    (let ((pc (get-register eceval 'pc))
          (original-pc-contents (get-register-contents eceval 'pc)))
      (set-contents! pc instructions) ; コンパイルしたコードが実行されるようpcを横取りする
      (execute) ; assembleしただけではコードは実行されないので,executeを実行する
      (set-contents! pc original-pc-contents) ; 元々のpcに処理を返す
      (get-register-contents eceval 'val) ; 最後に基本手続きcompile-and-runの返す値として,現在のvalの値を返す
      )))

(set-cdr! (last-pair primitive-procedures)
          (list (list 'compile-and-run compile-and-run)))

(define (main args)
  (start-eceval)
  ;=>
  ; ;; EC-Eval input:
  ; (compile-and-run
  ;   '(define (factorial n)
  ;      (if (= n 1)
  ;        1
  ;        (* (factorial (- n 1)) n))))

  ; (total-pushes = 5 maximum-depth = 3)
  ; ;; EC-Eval value:
  ; ok

  ; ;; EC-Eval input:
  ; (factorial 5)

  ; (total-pushes = 31 maximum-depth = 14)
  ; ;; EC-Eval value:
  ; 120

  ; ;; EC-Eval input:
  ; (define (factorial n)
  ;   (if (= n 1)
  ;     1
  ;     (* (factorial (- n 1)) n)))

  ; (total-pushes = 3 maximum-depth = 3)
  ; ;; EC-Eval value:
  ; ok

  ; ;; EC-Eval input:
  ; (factorial 5)

  ; (total-pushes = 144 maximum-depth = 28)
  ; ;; EC-Eval value:
  ; 120
  )
