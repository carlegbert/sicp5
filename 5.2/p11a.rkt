#lang sicp

(controller
 ...
 afterfib-n-2
 (assign n (reg val))
 (restore val)
 )

;; becomes

(controller
 ...
 afterfib-n-2
 (restore n)
 )
