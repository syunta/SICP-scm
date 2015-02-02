; a

(define (make-semaphore n)
  (let ((avail n)
        (mutex (make-mutex)))
    (define (acquire)
      (mutex 'acquire)
      (cond ((1 <= avail)
             (set! avail (- avail 1)))
            (else (mutex 'release)
                  (acquire))) ;retry
      (mutex 'release))
    (define (release)
      (mutex 'acquire)
      (set! avail (+ avail 1))
      (mutex 'release))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))
    the-semaphore))

; b

(define (make-semaphore n)
  (let ((avail n)
        (cell (list #f)))
    (define (acquire)
      (if (test-and-set! cell)
        (acquire) ;retry
        (cond ((1 <= avail)
               (set! avail (- avail 1))
               (clear! cell))
              (else (clear! cell)
                    (acquire))))) ;retry
    (define (release)
      (cond ((test-and-set! cell)
             (release)) ; retry
            (else (set! avail (+ avail 1))
                  (clear! cell))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))
    the-semaphore))
