(load "./2.38")

(define (reverser sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (main args)
  (print (reverser '(1 2 3 4 5))) 
  (print (reversel '(1 2 3 4 5))) 
  )
