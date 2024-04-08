;; (define (expt b n)
;;   (define (expt-iter counter product)
;;     (if (= counter 0)
;;       product
;;       (expt-iter (- counter 1) (* b product))))
;;   (expt-iter n 1))

(controller
  (assign p (const 1))
  expt-test
    (test (op =) (reg n) (const 0))
    (branch (label expt-done))
    (assign n (op -) (reg n) (const 1))
    (assign p (op *) (reg b) (reg p))
    (goto (label expt-test))
  expt-done)
