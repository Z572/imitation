(define-module (meson compiler)
  #:use-module (meson types)
  #:use-module (oop goops)
  #:export-syntax (define-compiler)
  #:export (lookup-compiler))

(define-syntax-rule (define-compiler class-name one
                      body ...)
  (begin (define-class* class-name (<compiler>))
         (export class-name)
         (define-public one (make class-name))))

(define (lookup-compiler name)
  (let* ((name (if (string? name) (string->symbol name)))
         (m (resolve-module `(meson compiler ,name))))
    (if (module-bound? m name)
        (module-ref m name)
        (error "no such compiler" name))))
