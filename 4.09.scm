(load "./eval-apply")
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

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

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

; while
; expr body …

; まずexprが評価され、もしそれが真値を返したら body … が評価されます。そしてexprが真値を返す 限り繰り返されます。

(define while-expression
  '(let ((a '(0 1 2 3 4)))
     ;**example of use**
     (while (pair? a)
            (print (pop! a)))))

(define while-derived
  '(let ((a '(0 1 2 3 4)))
     ;**derived expression**
     (let loop ()
       (if (pair? a)
         (begin
           (begin
             (print (pop! a)))
           (loop))
         #t))))

(define (while-expr exp) (cadr exp))
(define (while-body exp) (cddr exp))

(define (while->named-let exp)
  (make-named-let 'loop
                  '()
                  (make-if (while-expr exp)
                           (make-begin (list (make-begin (while-body exp))
                                             (list 'loop)))
                           true)))

; until
; expr body …

; while の条件を逆にしたものです。 つまり、 最初の形式ではexprが真値を返すまで body … を 繰り返します。

(define until-expression
  '(let ((a '(0 1 2 3 4)))
     ;**example of use**
     (until (null? a)
            (print a)
            (print (pop! a)))))

(define until-derived
  '(let ((a '(0 1 2 3 4)))
     ;**derived expression**
     (let loop ()
       (if (null? a)
         #t
         (begin
           (print a)
           (print (pop! a))
           (loop))))))

(define (until-expr exp) (cadr exp))
(define (until-body exp) (cddr exp))

(define (until->named-let exp)
  (make-named-let 'loop
                  '()
                  (make-if (until-expr exp)
                           true
                           (make-begin (append (until-body exp)
                                               (list (list 'loop)))))))

(define (main args)
  (print (equal? (do->named-let do-expression) do-derived))
  ;=> #t
  (print (equal? (while->named-let (caddr while-expression)) (caddr while-derived)))
  ;=> #t
  (print (equal? (until->named-let (caddr until-expression)) (caddr until-derived)))
  ;=> #t
  )
