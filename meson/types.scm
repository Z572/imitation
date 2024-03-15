
(define-module (meson types)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export (<compiler>
            <c-compiler>
            <meson>
            <env>
            <configuration-data>
            <dependency>
            <file>
            configuration.table
            .version
            .meson-version
            .variables
            .languages
            .license
            .options))
(define-class <compiler> ())
(define-class <c-compiler> (<compiler>))
(define-class <dependency> ()
  (name #:getter .name #:init-keyword #:name))
(define-method (write (d <dependency>) port)
  (format port "#<dependency '~a' ~x>" (.name d) (object-address d) ))
(define (make-meson-default-optional-table)
  (define table(make-hash-table))
  (hash-set! table "prefix" "/usr")
  (hash-set! table "libdir" "/lib")
  table)
(define-class <meson> ()
  (version #:accessor .version #:init-value #f)
  (license #:accessor .license #:init-value #f)
  (meson-version #:accessor .meson-version #:init-value "1.1.1")
  (options #:accessor .options #:init-form (make-meson-default-optional-table))
  (variables #:getter .variables #:init-form (make-module))
  (languages #:accessor .languages #:init-value '()))
(define-method (initialize (m <meson>) opt)
  (let ((o (next-method)))
    (module-define! (.variables o) 'meson m)
    o))
(define-class <env> ())
(define-class <file> ())
(define-class <configuration-data> ()
  (readonly? #:init-value #f #:accessor conf-data-readonly?)
  (table #:init-thunk make-hash-table #:getter configuration.table))
(define-public %meson (make-parameter (make <meson>)))
