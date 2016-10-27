(use gl)
(use gl.glut)
(load "./picture-language-support")
(load "./2.44")
;(load "./2.45")
(load "./2.46")
(load "./2.47")
(load "./2.48")
(load "./2.49")
(load "./2.50")
(load "./2.51")

(define frame (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode GLUT_RGBA)
  ;(glut-init-window-size 350 240)
  (glut-create-window "picture language")
  (glut-display-func display)
  (init)
  (glut-main-loop)
  )

(define (display)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color 0.0 0.0 0.0)
  (gl-begin GL_LINES)

  ;; 2.44
  ;((up-split wave 4) frame)
  ;; 2.49
  ;(painter-a frame)
  ;(painter-b frame)
  ;(painter-c frame)
  ;(wave frame)
  ;; 2.50
  ;((flip-horiz wave) frame)
  ;((rotate180 wave) frame)
  ;((rotate270 wave) frame)
  ;; 2.51
  ;((below wave wave) frame)
  ((below2 wave wave) frame)

  (gl-end)
  (gl-flush)
  )

(define (init)
  (gl-clear-color 1.0 1.0 1.0 1.0)
  )

(define (draw-line v1 v2)
  (define (t z)
    (- (* 2 z) 1))
  (gl-vertex (t (xcor-vect v1)) (t (ycor-vect v1)))
  (gl-vertex (t (xcor-vect v2)) (t (ycor-vect v2)))
  )
