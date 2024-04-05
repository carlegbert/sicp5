;; good-enough & improve as primitives
(controller
  (assign g (const 1.0))

  test-guess
  (test (op good-enough?) (reg g))
  (branch (label sqrt-done))
  (assign g (op improve) (reg g))
  (goto (label test-guess))

  sqrt-done)

;; inlined
(controller
  (assign g (const 1.0))

  test-guess
  (assign d (op /) (reg x) (reg g))
  (assign s (op +) (reg g) (reg d))
  (assign g (op /) (reg s) (const 2))

  (assign sq (op *) (reg g) (reg g))
  (test (op >) (reg sq) (const 0))
  (branch (label abs-done))
  (assign sq (op *) (reg sq) (const -1))
  abs-done

  (test (op <) (reg a) (const 0.001))
  (branch (label sqrt-done))

  (goto (label test-guess))

  sqrt-done)
