; a
(define laughing-wave
  (segments->painter (list (make-segment (make-vect 0.25 0) (make-vect 0.33 0.5))
                           (make-segment (make-vect 0.33 0.5) (make-vect 0.25 0.55))
                           (make-segment (make-vect 0.25 0.55) (make-vect 0.15 0.45))
                           (make-segment (make-vect 0.15 0.45) (make-vect 0 0.65))
                           (make-segment (make-vect 0 0.85) (make-vect 0.15 0.60))
                           (make-segment (make-vect 0.15 0.60) (make-vect 0.25 0.65))
                           (make-segment (make-vect 0.25 0.65) (make-vect 0.40 0.65))
                           (make-segment (make-vect 0.40 0.65) (make-vect 0.35 0.85))
                           (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1))
                           (make-segment (make-vect 0.60 1) (make-vect 0.65 0.85))
                           (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))
                           (make-segment (make-vect 0.60 0.65) (make-vect 0.75 0.65))
                           (make-segment (make-vect 0.75 0.65) (make-vect 1 0.40))
                           (make-segment (make-vect 1 0.20) (make-vect 0.63 0.45))
                           (make-segment (make-vect 0.63 0.45) (make-vect 0.75 0))
                           (make-segment (make-vect 0.60 0) (make-vect 0.5 0.20))
                           (make-segment (make-vect 0.5 0.20) (make-vect 0.40 0))
                           (make-segment (make-vect 0.4 0.75) (make-vect 0.45 0.7))
                           (make-segment (make-vect 0.6 0.75) (make-vect 0.55 0.7))
                           (make-segment (make-vect 0.45 0.7) (make-vect 0.55 0.7))
                           (make-segment (make-vect 0.4 0.75) (make-vect 0.6 0.75))
                           )))

; b
(define (corner-split-2 painter n)
  (if (= n 0)
    painter
    (let ((top-left (up-split painter (- n 1)))
          (bottom-right (right-split painter (- n 1)))
          (corner (corner-split-2 painter (- n 1))))
      (beside (below painter top-left)
              (below bottom-right corner)))))

; c
(define (square-limit-2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))
