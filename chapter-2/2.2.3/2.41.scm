(load "./2.40")

(define (unique-triples n)
  (flatmap (lambda (ij)
             (map (lambda (k) (append ij (list k)))
                  (enumerate-interval 1 (- (cadr ij) 1))))
           (unique-pairs n)))

(define (triple-sum-equal s n)
  (filter (lambda (ijk)
            (= s (accumulate + 0 ijk)))
          (unique-triples n)))

(define (main args)
  (print (unique-triples 5))
  (print (triple-sum-equal 10 5))
  )
