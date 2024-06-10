#lang racket

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (list
                   (stack-inst-reg-name inst)
                   (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let* ((item (pop stack))
             (saved-reg-name (car stack))
             (val (cadr stack)))
        (if (eq? reg saved-reg-name
                 (begin
                   (set-contents! reg (pop stack))
                   (advance-pc pc))
                 (error "ERROR -- mismatched register")))))))


