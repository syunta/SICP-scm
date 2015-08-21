(load "../../lib/eval-apply")
(load "../../lib/library")
; do 
; ((variable init [step]) …) (test expr …) body …

; 1. init …を評価し、variable …をそれぞれの結果へと 束縛します。以降のステップはvariable …が束縛された環境で評価されます。
; 2. testを評価します。真の値が得られたら、expr …を順に評価し、最後のexprの結果を返り値とします。
; 3. そうでなければ、body …を(副作用のために)評価します。
; 4. それからstep …を評価し、それぞれの結果へ 新たなvariable …を束縛し、ステップ2から繰り返します。

(define do-expression
  ;**example of use**
  '(do ((i 0 (+ i 1))
        (j 0 (+ i j)))
     ((= i 10) j)
     (print j)))

(define do-derived
  ;**derived expression**
  '(let loop ((i 0) (j 0))
     (if (= i 10)
       (begin
         j)
       (begin
         (begin
           (print j))
         (loop (+ i 1) (+ i j))))))

(define (do-counter exp) (cadr exp))
(define (do-variables exp) (map car (do-counter exp)))
(define (do-inits exp) (map cadr (do-counter exp)))
(define (do-steps exp) (flatmap cddr (do-counter exp)))

(define (do-cond exp) (caddr exp))
(define (do-test exp) (car (do-cond exp)))
(define (do-exprs exp) (cdr (do-cond exp)))

(define (do-body exp) (cdddr exp))

(define (do->named-let exp)
  (make-named-let 'loop
                  (map (lambda (var init) (list var init))
                       (do-variables exp)
                       (do-inits exp))
                  (make-if (do-test exp)
                           (make-begin (do-exprs exp))
                           (make-begin (list (make-begin (do-body exp))
                                             (cons 'loop (do-steps exp)))))))

(define (main args)
  (print (equal? (do->named-let do-expression) do-derived)))
