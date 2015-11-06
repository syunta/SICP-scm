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

(define (safe? x)
  (and (distinct? x)
       (not (= (baker x) 5))
       (not (= (cooper x) 1))
       (not (= (fletcher x) 5))
       (not (= (fletcher x) 1))
       (> (miller x) (cooper x))
       (not (= (abs (- (smith x) (fletcher x))) 1))
       (not (= (abs (- (fletcher x) (cooper x))) 1))))

(define (baker seq) (car seq))
(define (cooper seq) (cadr seq))
(define (fletcher seq) (caddr seq))
(define (miller seq) (cadddr seq))
(define (smith seq) (car (cddddr seq)))

(define (multiple-dwelling)
  (map (lambda (x)
         (list (list 'baker (baker x))
               (list 'cooper (cooper x))
               (list 'fletcher (fletcher x))
               (list 'miller (miller x))
               (list 'smith (smith x))))
       (filter safe?
               (let ((baker '(1 2 3 4 5))
                     (cooper '(1 2 3 4 5))
                     (fletcher '(1 2 3 4 5))
                     (miller '(1 2 3 4 5))
                     (smith '(1 2 3 4 5)))
                 (quintuples baker cooper fletcher miller smith)))))

; 前段の制限で既に除外されたのではない可能性だけの生成を基にした解き方
; より効率的になったはず
(define (adjoin x y) (append x (list y)))

(define (adjoin-possibilities new-ps ps)
  (flatmap
    (lambda (p)
      (map (lambda (new-p) (adjoin p new-p))
           new-ps))
    ps))

(define (multiple-dwelling-improved)
  (let ((baker-possibility '(1 2 3 4 5))
        (cooper-possibility '(1 2 3 4 5))
        (fletcher-possibility '(1 2 3 4 5))
        (miller-possibility '(1 2 3 4 5))
        (smith-possibility '(1 2 3 4 5)))
    (map
      (lambda (x)
        (list (list 'baker (baker x))
              (list 'cooper (cooper x))
              (list 'fletcher (fletcher x))
              (list 'miller (miller x))
              (list 'smith (smith x))))
      (filter
        distinct?
        (filter
          (lambda (x) (not (= (abs (- (smith x) (fletcher x))) 1)))
          (adjoin-possibilities
            smith-possibility
            (filter
              (lambda (x) (not (= (abs (- (fletcher x) (cooper x))) 1)))
              (filter
                (lambda (x) (> (miller x) (cooper x)))
                (adjoin-possibilities
                  miller-possibility
                  (filter
                    (lambda (x) (not (= (fletcher x) 1)))
                    (filter
                      (lambda (x) (not (= (fletcher x) 5)))
                      (adjoin-possibilities
                        fletcher-possibility
                        (filter
                          (lambda (x) (not (= (cooper x) 1)))
                          (adjoin-possibilities
                            cooper-possibility
                            (filter
                              (lambda (x) (not (= (baker x) 5)))
                              (map
                                (lambda (x) (list x))
                                baker-possibility))))))))))))))))

(define (main args)
  (print (multiple-dwelling))
  (print (multiple-dwelling-improved))
  ;=>
  ; (((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
  ; (((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
  )
