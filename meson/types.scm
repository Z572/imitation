
(define-module (meson types)
  #:use-module (oop goops)
  #:export (<compiler>
            <c-compiler>
            <meson>
            <env>
            <configuration-data>
            configuration.table))
(define-class <compiler> ())
(define-class <c-compiler> (<compiler>))
(define-class <meson> ())
(define-class <env> ())
(define-class <configuration-data> ()
  (readonly? #:init-value #f #:accessor conf-data-readonly?)
  (table #:init-thunk make-hash-table #:getter configuration.table))
