(load "./2.40")

(define (unique-triples n)
  (flatmap (lambda (ij)
             (map (lambda (k) (append ij (list k)))
                  (enumerate-interval 1 (- (cadr ij) 1))))
           (unique-pairs n)))

(define (unique-ns d n)
  (if (< d 3)
    (flatmap (lambda (i)
                 (map (lambda (j) (list i j))
                      (enumerate-interval 1 (- i 1))))
               (enumerate-interval 2 n))
    (flatmap (lambda (rest)
               (map (lambda (new) (append rest (list new)))
                    (enumerate-interval 1 (- (list-ref rest (- d 2)) 1))))
             (unique-ns (- d 1) n))))

(define (triple-sum-equal s n)
  (filter (lambda (ijk)
            (= s (accumulate + 0 ijk)))
          (unique-triples n)))

(define (main args)
  (print (unique-triples 5))
  (print (unique-ns 3 5))
  (print (triple-sum-equal 10 5))
  )
