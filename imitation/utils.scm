(define-module (imitation utils)
  #:export (ensure-list
            with-directory-excursion))

(define-syntax-rule (with-directory-excursion dir body ...)
  "Run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
    (dynamic-wind
      (lambda ()
        (chdir dir))
      (lambda ()
        body ...)
      (lambda ()
        (chdir init)))))

(define (ensure-list o)
  (if (list? o)
      o
      (list o)))
