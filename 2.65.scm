(load "./2.64")

(define tree->list tree->list-2)

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

; union-flat も intersection-flat も list->tree も tree->listも、O(n)なので、
; O(n) + O(n) + O(n) = O(n) という戦略

(define (intersection-set tree1 tree2)
  (list->tree
    (intersection-flat (tree->list tree1)
                       (tree->list tree2))))

(define (union-set tree1 tree2)
  (list->tree
    (union-flat (tree->list tree1)
                (tree->list tree2))))

(define (main args)
  (print (union-set (list->tree '(1 3 5)) (list->tree '(2 4 6))))
  (print (intersection-set (list->tree '(1 3 4 5 6)) (list->tree '(2 3 5 6))))
  )
