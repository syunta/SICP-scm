; queueを使ってのシミュレーション
(load "./simulator.scm")
(load "./queue.scm")

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define the-agenda (make-agenda))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(probe 'a a)
(probe 'b b)
(probe 'c c)

(and-gate a b c)
; (0 (3 . (#c=0 #c=0))) : the-agendaの状態
(set-signal! a 0)
(set-signal! b 1)
; (0 (3 . (#c=0 #c=0 #c=0)))
(propagate)

(set-signal! a 1)
(set-signal! b 0)
; (3 (6 . (#c=0 #c=1)))
(propagate)
; #c=1 => #c=0 の順番で処理され、正しい結果を印字する。
;=>
; a 0  New-value = 0
; b 0  New-value = 0
; c 0  New-value = 0
; b 0  New-value = 1
; a 3  New-value = 1
; b 3  New-value = 0
; c 6  New-value = 1
; c 6  New-value = 0


; stackを使ってのシミュレーション
(load "./stack.scm")
(define empty-queue? empty-stack?)
(define front-queue front-stack)
(define insert-queue! insert-stack!)
(define delete-queue! delete-stack!)
(define make-queue make-stack)

(define the-agenda (make-agenda))

(define A (make-wire))
(define B (make-wire))
(define C (make-wire))

(probe 'A A)
(probe 'B B)
(probe 'C C)

(and-gate A B C)
; (0 (3 . (#C=0 #C=0)))
(set-signal! A 0)
(set-signal! B 1)
; (0 (3 . (#C=0 #C=0 #C=0)))
(propagate)

(set-signal! A 1)
(set-signal! B 0)
; (3 (6 . (#C=0 #C=1)))
(propagate)
; #C=0 => #C=1 の順番で処理されてしまい、誤った結果を印字する。
;=>
; A 0  New-value = 0
; B 0  New-value = 0
; C 0  New-value = 0
; B 0  New-value = 1
; A 3  New-value = 1
; B 3  New-value = 0
; C 6  New-value = 1
