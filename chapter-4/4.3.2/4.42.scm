(add-load-path "../../lib" :relative)
(load "amb")

(for-each simple-ambeval
          '((define (distinct? items)
              (cond ((null? items) treu)
                    ((null? (cdr items)) true)
                    ((member (car items) (cdr items)) false)
                    (else (distinct? (cdr items)))))

            (define (liar-puzzle)
              (define (betty x) (car x))
              (define (ethel x) (cadr x))
              (define (joan x) (caddr x))
              (define (kitty x) (cadddr x))
              (define (mary x) (car (cddddr x)))

              (define (fact x) (car x))
              (define (fiction x) (cadr x))

              (define (betty-1 x) (= (kitty x) 2))
              (define (betty-2 x) (= (betty x) 3))
              (define (ethel-1 x) (= (ethel x) 1))
              (define (ethel-2 x) (= (joan x) 2))
              (define (joan-1 x) (= (joan x) 3))
              (define (joan-2 x) (= (ethel x) 5))
              (define (kitty-1 x) (= (kitty x) 2))
              (define (kitty-2 x) (= (mary x) 4))
              (define (mary-1 x) (= (mary x) 4))
              (define (mary-2 x) (= (betty x) 1))

              (let ((betty-letter (amb (list betty-1 betty-2) (list betty-2 betty-1)))
                    (ethel-letter (amb (list ethel-1 ethel-2) (list ethel-2 ethel-1)))
                    (joan-letter (amb (list joan-1 joan-2) (list joan-2 joan-1)))
                    (kitty-letter (amb (list kitty-1 kitty-2) (list kitty-2 kitty-1)))
                    (mary-letter (amb (list mary-1 mary-2) (list mary-2 mary-1))))
                (let ((betty (amb 1 2 3 4 5))
                      (ethel (amb 1 2 3 4 5))
                      (joan (amb 1 2 3 4 5))
                      (kitty (amb 1 2 3 4 5))
                      (mary (amb 1 2 3 4 5)))
                  (let ((answer (list betty ethel joan kitty mary)))
                    (require ((fact betty-letter) answer))
                    (require ((fact ethel-letter) answer))
                    (require ((fact joan-letter) answer))
                    (require ((fact kitty-letter) answer))
                    (require ((fact mary-letter) answer))
                    (require (not ((fiction betty-letter) answer)))
                    (require (not ((fiction ethel-letter) answer)))
                    (require (not ((fiction joan-letter) answer)))
                    (require (not ((fiction kitty-letter) answer)))
                    (require (not ((fiction mary-letter) answer)))
                    (require (distinct? answer))
                    (list (list 'betty betty)
                          (list 'ethel ethel)
                          (list 'joan joan)
                          (list 'kitty kitty)
                          (list 'mary mary))))))
            ))

(define (main args)
  (print-ambeval '(liar-puzzle) 100)
  ;=>
  ; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
  ; End of serch
  )