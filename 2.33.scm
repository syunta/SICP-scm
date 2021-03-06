(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (_ acc) (+ 1 acc))
              0
              sequence))

(define (main args)
  (print (map1 square '(1 2 3 4)))
  (print (append '(1 2) '(3 4)))
  (print (length '(1 2 3 4)))
  )
