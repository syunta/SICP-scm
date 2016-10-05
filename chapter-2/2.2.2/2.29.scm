(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b
(define (weight? structure) (number? structure))

(define (count-weight branch)
  (let ((structure (branch-structure branch)))
    (if (weight? structure)
      structure
      (total-weight structure))))

(define (total-weight mobile)
  (+ (count-weight (left-branch mobile))
     (count-weight (right-branch mobile))))

; c
(define the-empty-mobile '())
(define the-empty-mobile? null?)

(define (balanced? structure)
  (if (weight? structure)
    #t
    (let ((left (left-branch structure))
          (right (right-branch structure)))
      (and (= (torque left)
              (torque right))
           (balanced? (branch-structure left))
           (balanced? (branch-structure right))))))

(define (torque branch)
  (* (branch-length branch)
     (count-weight branch)))

; d
(define (make-mobile-2 left right)
  (cons left right))
(define (make-branch-2 length structure)
  (cons length structure))

(define (right-branch-2 mobile) (cdr mobile))
(define (branch-structure-2 branch) (cdr branch))
; 構成子の表現が変更されても選択子を変更するだけでよい。

; test
(define mobile
  (make-mobile (make-branch 1 3)
               (make-branch 1
                            (make-mobile (make-branch 1 3)
                                         (make-branch 2 3)))))

(define mobile-balanced
  (make-mobile (make-branch 2 3)
               (make-branch 1
                            (make-mobile (make-branch 1 3)
                                         (make-branch 1 3)))))

(define mobile-2
  (make-mobile-2 (make-branch-2 1 3)
                 (make-branch-2 1
                                (make-mobile-2 (make-branch-2 1 3)
                                               (make-branch-2 2 3)))))

(define mobile-balanced-2
  (make-mobile-2 (make-branch-2 2 3)
                 (make-branch-2 1
                                (make-mobile-2 (make-branch-2 1 3)
                                               (make-branch-2 1 3)))))

(define (main args)
  (print (total-weight mobile))
  (print (balanced? mobile))
  (print (balanced? mobile-balanced))

  (set! right-branch right-branch-2)
  (set! branch-structure branch-structure-2)

  (print (total-weight mobile-2))
  (print (balanced? mobile-2))
  (print (balanced? mobile-balanced-2))
  )
