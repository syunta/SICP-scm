(add-load-path "../.." :relative)
(load "lib/library.scm")

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      nil
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree))
            (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    nil
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)   ;記号
                             (cadr pair)) ;頻度
                  (make-leaf-set (cdr pairs))))))

;(print (make-leaf-set '((A 4) (B 3) (C 1) (D 1))))

;2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(print (decode sample-message sample-tree))
;(print sample-tree)

;2.68
(define (encode message tree)
  (if (null? message)
    nil
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol message tree)
  (define (iter message tree result)
    (cond ((leaf? tree) (reverse result))
          ((memq message (symbols (left-branch tree)))
           (iter message (left-branch tree) (cons 0 result)))
          ((memq message (symbols (right-branch tree)))
           (iter message (right-branch tree) (cons 1 result)))
          (else (error "Symbol does not exists!" message))))
  (iter message tree nil))

;(print sample-message)
;(print (encode '(A D A B B C A) sample-tree))

;2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
    (if (null? (cdr leaves))
      (car leaves)
      (let ((leaf1 (car leaves)) (leaf2 (cadr leaves)) (rest (cddr leaves)))
        (successive-merge (adjoin-set (make-code-tree leaf2 leaf1) rest)))))

;(print (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1) (E 5) (F 6))))

;2.70
(define song
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
    SHA BOOM))

(define song-list
  '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(print (length (encode song (generate-huffman-tree song-list))))
(print (* 3 (length song)))

;2.71
(define n=5
  '((A 1) (B 2) (C 4) (D 8) (E 16)))
(print (generate-huffman-tree n=5))
(print (encode '(E) (generate-huffman-tree n=5))) ;最高頻度
(print (encode '(A) (generate-huffman-tree n=5))) ;最低頻度

(define n=10
  '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
(print (generate-huffman-tree n=10))
(print (encode '(J) (generate-huffman-tree n=10))) ;最高頻度
(print (encode '(A) (generate-huffman-tree n=10))) ;最低頻度

;2.72
;最高頻度の増加の程度
;O(1)

;最低頻度の増加の程度
;O(n)
