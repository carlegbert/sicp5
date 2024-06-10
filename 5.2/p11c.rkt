#lang racket

(define (pop stack reg)
  (stack 'pop reg))

(define (make-stack reg-names)
  (let ((stack-collection '()))
    (define (push x reg)
      (let ((s (assoc reg stack-collection)))
        (set-cdr! s (cons x (cdr stack-collection)))))
    (define (pop reg)
      (let ((s (assoc reg stack-collection)))
        (if (null? (cdr s))
            (error "Empty stack -- POP")
            (let ((top (cadr s)))
              (set-cdr! s (cddr s))
              top))))
    (define (initialize)
      (set! stack-collection
            (map (lambda (reg) (list reg))
                 reg-names))
      'done)
    (define (dispatch message arg)
      (cond ((eq? message 'push) (push arg))
            ((eq? message 'pop) (pop arg))
            ((eq? message 'initialize) (initialize arg))
            (else
             error "Unknown request -- STACK"
             message)))
    dispatch))

;; Some additional trivial updates to be made:
;; make-new-machine will be updated to accept register names
;; (which is already available where make-new-machine is called).
;; The top-level pop, as well as its caller make-restore will
;; need to take the register name into consideration (already in scope in make restore).
;; The identical change would need to be made in the top-level push & its caller make-save.
