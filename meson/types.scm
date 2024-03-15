
(define-module (meson types)
  #:use-module (oop goops)
  #:export (<compiler>
            <c-compiler>
            <meson>
            <env>
            <configuration-data>
            configuration.table
            .version
            .depends))
(define-class <compiler> ())
(define-class <c-compiler> (<compiler>))
(define-class <meson> ()
  (depends #:accessor .depends #:init-value '())
  (version #:accessor .version #:init-value "1.1.1"))
(define-class <env> ())
(define-class <configuration-data> ()
  (readonly? #:init-value #f #:accessor conf-data-readonly?)
  (table #:init-thunk make-hash-table #:getter configuration.table))
(define-public %meson (make-parameter (make <meson>)))
