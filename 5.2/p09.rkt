(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                 (if (or (register-exp? exp) (label-exp? exp))
                   (error "Not an operation expression --" exp)
                   (make-primitive-exp e machine labels))
                 (operation-exp-operands exp)))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; the following might also work but I'd probably
;; prefer erroring while making the expression.
(define (operation-exp? exp)
  (and (pair? exp)
       (not (register-exp? exp))
       (not (label-exp? exp))
       (tagged-list? (car exp) 'op)))

