(define-module (imitation utils)
  #:export (ensure-list))
(define (ensure-list o)
  (if (list? o)
      o
      (list o)))
