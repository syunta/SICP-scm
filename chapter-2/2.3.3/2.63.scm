(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define (main args)
  (define set1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
  (define set2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
  (define set3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
  ; a
  ; 同じ結果を生じる
  (print (equal? (tree->list-1 set1) (tree->list-2 set1)))
  (print (equal? (tree->list-1 set2) (tree->list-2 set2)))
  (print (equal? (tree->list-1 set3) (tree->list-2 set3)))

  ; b
  ; tree->list(copy-to-list)の再帰呼び出しの増加の程度は同じ
  ; ただし、appendを使っている分、tree->list-1の方がリスト走査の増加の程度は高い
  )
