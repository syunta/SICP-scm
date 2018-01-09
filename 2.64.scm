(load "./2.63")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (ash (- n 1) -1)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

(define (main args)
  ; a
  ;
  ; 左部分木から順に作っていく深さ優先探索的な作り方
  (print (list->tree '(1 3 5 7 9 11)))
  ;=> (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
  ;
  ; 5 ----> 9 -> 11
  ; |       |
  ; V       V
  ; 1 -> 3  7

  ; b
  ; partial-treeの中ではcar, cdr のみで、特にリストを走査している訳ではない。
  ; partial-treeはだいたいリストの要素数分呼び出される
  ; よって Θ(n)
  )
