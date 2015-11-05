(load "../../lib/library")

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(define (pairs xs ys)
  (flatmap (lambda (x)
             (map (lambda (y)
                    (list x y))
                  ys))
           xs))

(define (triples xs ys zs)
  (flatmap (lambda (xy)
             (map (lambda (z)
                    (append xy (list z)))
                  zs))
           (pairs xs ys)))

(define (quadruples xs ys zs ws)
  (flatmap (lambda (xyz)
             (map (lambda (w)
                    (append xyz (list w)))
                  ws))
           (triples xs ys zs)))

(define (quintuples xs ys zs ws ps)
  (flatmap (lambda (xyzw)
             (map (lambda (p)
                    (append xyzw (list p)))
                  ws))
           (quadruples xs ys zs ws)))

(define (baker seq) (car seq))
(define (cooper seq) (cadr seq))
(define (fletcher seq) (caddr seq))
(define (miller seq) (cadddr seq))
(define (smith seq) (car (cddddr seq)))

(define (multiple-dwelling)
  (map
    (lambda (x)
      (list (list 'baker (baker x))
            (list 'cooper (cooper x))
            (list 'fletcher (fletcher x))
            (list 'miller (miller x))
            (list 'smith (smith x))))
    (filter
      (lambda (x) (not (= (abs (- (fletcher x) (cooper x))) 1)))
      (filter
        (lambda (x) (not (= (abs (- (smith x) (fletcher x))) 1)))
        (filter
          (lambda (x) (> (miller x) (cooper x)))
          (filter
            (lambda (x) (not (= (fletcher x) 1)))
            (filter
              (lambda (x) (not (= (fletcher x) 5)))
              (filter
                (lambda (x) (not (= (cooper x) 1)))
                (filter
                  (lambda (x) (not (= (baker x) 5)))
                  (filter distinct?
                          (let ((baker '(1 2 3 4 5))
                                (cooper '(1 2 3 4 5))
                                (fletcher '(1 2 3 4 5))
                                (miller '(1 2 3 4 5))
                                (smith '(1 2 3 4 5)))
                            (quintuples baker cooper fletcher miller smith))))))))))))

(define (main args)
  (print (multiple-dwelling))
  ;=>
  ; (((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
  )
