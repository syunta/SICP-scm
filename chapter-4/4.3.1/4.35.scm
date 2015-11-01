(add-load-path "../../lib" :relative)
(load "amb")

(for-each simple-ambeval
          '((define (enumerate-interval low high)
              (if (> low high)
                '()
                (cons low (enumerate-interval (+ 1 low) high))))

            (define (an-integer-between low high)
              (an-element-of (enumerate-interval low high)))

            (define (a-pythagorean-triple-between low high)
              (let ((i (an-integer-between low high)))
                (let ((j (an-integer-between i high)))
                  (let ((k (an-integer-between j high)))
                    (require (= (+ (* i i) (* j j)) (* k k)))
                    (list i j k)))))))

(define (main args)
  (print-ambeval '(a-pythagorean-triple-between 1 15) 10)
  ;=>
  ; (3 4 5)
  ; (5 12 13)
  ; (6 8 10)
  ; (9 12 15)
  ; End of search
  )
