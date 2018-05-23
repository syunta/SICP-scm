(controller
  (assign product (const 1))
  (assign counter (const 1))
  test-counter
    (test (op >) (reg counter) (reg n))
    (branch (label factorial-done)
    (assign product (op *) (reg counter) (reg product))
    (assign counter (op +) (reg counter) (const 1))
    (goto (label test-counter))
  factorial-done)
