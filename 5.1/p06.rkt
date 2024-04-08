(controller
  assign continue (label fib-done)
  fib-loop
    (test (op < (reg n) (const 2)))
    (branch (label immediate-answer))
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)
    (assign n (op -) (reg n) (const 1))
    (goto (label fib-loop))
  afterfib-n-1
    (restore n)
    (restore continue) ;; <--- HERE
    (assign n (op -) (reg n) (const 2))
    (save continue) ;; <--- HERE
    (assign continue (label afterfib-n-2))
    ;; etc...
    )
