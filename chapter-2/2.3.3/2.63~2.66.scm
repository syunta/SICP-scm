(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
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

; 後の問題で使いたいからtree->listに名前変えてある
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

;2.63
; a.の答え
; 同じ結果を生じると思う
; 少なくとも図.2.16 の木からは同じ結果を生じる ↓がその結果
(print (tree->list   '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))))
(print (tree->list   '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))))
(print (tree->list   '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))))
(print (tree->list-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))))
(print (tree->list-1 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))))
(print (tree->list-1 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))))

; b.の答え
; ステップ数が何のことか明言してない気がするが、
; tree->list(copy-to-list)の再帰呼び出しの増加の程度ならおんなじ
; でもappend使ってる分tree->list-1の方が遅そう

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
;2.64
; a.の答え
; n = (1 2 3 4 5 6)
; (left 1 2) (entry 3) (right 4 5 6)
;   (left  (left) (entry 1) (right 2))
;     (right (left) (entry 2) (right))
;   (right (left 4) (entry 5) (right 6))
;     (left  (left) (entry 4) (right))
;     (right (left) (entry 6) (right))
;
; ↑こんな感じ
;
(print (list->tree '(1 3 5 7 9 11)))
; 図.2.16の右側に近い形、1と3の位置が入れ替わってる
;
; b.の答え
; 要素を一個増やすたびに、'(x () ()) みたいな形のリストが増える
; よって、関数の呼び出し回数=ステップ数とするなら、
; たぶん増加の程度は 2n

;2.65
; union-flatもintersection-flatもlist->treeもtree->listも、O(n)なので、
; O(n) + O(n) + O(n) = O(n) という戦略
(define (union-flat set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((= (car set1) (car set2))
           (union-flat (cdr set1) set2))
          ((< (car set1) (car set2)) 
           (cons (car set1) (union-flat (cdr set1) set2)))
          ((> (car set1) (car set2))
           (cons (car set2) (union-flat set1 (cdr set2))))))

(define (intersection-flat set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x (car set1)) (y (car set2)))
      (cond ((= x y)
             (cons x (intersection-flat (cdr set1)
                                       (cdr set2))))
            ((< x y)
             (intersection-flat (cdr set1) set2))
            ((> x y)
             (intersection-flat set1 (cdr set2)))))))

(define (intersection-set xs ys)
  (list->tree
    (intersection-flat (tree->list xs)
                       (tree->list ys))))

(define (union-set xs ys)
  (list->tree
    (union-flat (tree->list xs)
                (tree->list ys))))

; 2.66
(define personnel-db
  (list->tree
    '((1  ((name a) (age 26)))
      (2  ((name b) (age 24)))
      (3  ((name c) (age 28)))
      (5  ((name d) (age 26)))
      (8  ((name e) (age 26)))
      (11 ((name f) (age 23))))))

(define (key record)
  (caar record))

(define (contents record)
  (cadar record))

(define (lookup given-key db)
  (cond ((null? db) (error "Not found"))
        ((= given-key (key db)) (contents db))
        ((< given-key (key db))
         (lookup given-key (left-branch db)))
        ((> given-key (key db))
         (lookup given-key (right-branch db)))))

(print (lookup 1 personnel-db))
