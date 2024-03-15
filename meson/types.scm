
(define-module (meson types)
  #:use-module (oop goops)
  #:export (<compiler>
            <c-compiler>
            <meson>
            <env>
            <configuration-data>
            <dependency>
            configuration.table
            .version
            .meson-version
            .variables
            .languages
            .license))
(define-class <compiler> ())
(define-class <c-compiler> (<compiler>))
(define-class <dependency> ()
  (name #:getter .name #:init-keyword #:name))
(define-method (write (d <dependency>) port)
  (format port "#<dependency '~a' ~x>" (.name d) (object-address d) ))
(define-class <meson> ()
  (version #:accessor .version #:init-value #f)
  (license #:accessor .license #:init-value #f)
  (meson-version #:accessor .meson-version #:init-value "1.1.1")

  (variables #:getter .variables #:init-form (make-module))
  (languages #:accessor .languages #:init-value '()))
(define-class <env> ())
(define-class <configuration-data> ()
  (readonly? #:init-value #f #:accessor conf-data-readonly?)
  (table #:init-thunk make-hash-table #:getter configuration.table))
(define-public %meson (make-parameter (make <meson>)))
