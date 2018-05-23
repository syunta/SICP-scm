(load "./query")

; lives-near では、組み合わせを列挙しているだけだから。

; すでに作られた結果同士を比較して重複を排除すればよさそうだが、すでに作られた結果を比較する方法はない(と思われる).
; 実現するには、すでに作られた結果1つだけを見て重複を排除する必要がある。
; 結果の (lives-near ?person-1 ?person-2) がアルファベット順に並んでいなければならないという条件を加える、などの方法が考えられる。

(define (name<? n1 n2)
  (string<? (string-join (map symbol->string n1) " ")
            (string-join (map symbol->string n2) " ")))

(set! primitive-procedures (append primitive-procedures (list (list 'name<? name<?))))
(define user-initial-environment (setup-environment))

(qeval-add
  '(assert! (rule (lives-near-2 ?person-1 ?person-2)
                  (and (address ?person-1 (?town . ?rest-1))
                       (address ?person-2 (?town . ?rest-2))
                       (not (same ?person-1 ?person-2))
                       (lisp-value name<? ?person-1 ?person-2)))))

(define test '(lives-near-2 ?person-1 ?person-2))

(define (main args)
  (print-qeval test)
  ;=>
  ;(lives-near-2 (Aull DeWitt) (Reasoner Louis))
  ;(lives-near-2 (Aull DeWitt) (Bitdiddle Ben))
  ;(lives-near-2 (Fect Cy D) (Hacker Alyssa P))
  ;(lives-near-2 (Bitdiddle Ben) (Reasoner Louis))
  )
