(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define painter-a
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
                           (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 1 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1)))))

(define painter-b
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define painter-c
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                           (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 1)))))

(define wave
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
                           )))
