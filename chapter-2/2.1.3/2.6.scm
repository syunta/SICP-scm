(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

; one を導出するには (add-1 zero) の簡約を行えばよい

(add-1 zero)

(lambda (f)
  (lambda (x)
    (f ((zero f) x))))

(lambda (f)
  (lambda (x)
    (f (((lambda (f) (lambda (x) x)) f) x))))

(lambda (f)
  (lambda (x)
    (f ((lambda (x) x) x))))

(lambda (f)
  (lambda (x)
    (f x)))

; このように簡約できるので、one の定義は以下の通り

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

; 同様に two の導出には (add-1 one) 行えばよい

(add-1 one)

(lambda (f)
  (lambda (x)
    (f ((one f) x))))

(lambda (f)
  (lambda (x)
    (f (((lambda (f) (lambda (x) (f x))) f) x))))

(lambda (f)
  (lambda (x)
    (f ((lambda (x) (f x)) x))))

(lambda (f)
  (lambda (x)
    (f (f x))))

; このように簡約できるので、two の定義は以下の通り

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

; ここまでの結果から、次のことが見えてくる
;
; zero => f の評価が0回
; one  => f の評価が1回
; two  => f の評価が2回
;
; よって、+演算子は f の評価を重ねるように実装すればよさそう

(define (add n1 n2)
  (lambda (f)
    (lambda (x)
      ((n2 f) ((n1 f) x)))))

; 1 + 2 を簡約しこの定義が正しいかテストする

(add one two)

(lambda (f)
  (lambda (x)
    ((two f) ((one f) x))))

(lambda (f)
  (lambda (x)
    ((two f) (((lambda (f)
                 (lambda (x)
                   (f x)))
               f) x))))

(lambda (f)
  (lambda (x)
    ((two f) ((lambda (x)
                (f x))
              x))))

(lambda (f)
  (lambda (x)
    ((two f) (f x))))

(lambda (f)
  (lambda (x)
    (((lambda (f)
        (lambda (x)
          (f (f x))))
      f) (f x))))

(lambda (f)
  (lambda (x)
    ((lambda (x)
       (f (f x)))
     (f x))))

(lambda (f)
  (lambda (x)
    (f (f (f x)))))

; f の評価が3回の手続きに簡約できた

(define (lambda->int f)
  ((f (lambda (x) (+ x 1))) 0))

(define (print-lambda f)
  (print (lambda->int f)))

(define (main args)
  (print-lambda one)
  (print-lambda two)
  (print-lambda (add one two))
  )
