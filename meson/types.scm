
(define-module (meson types)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (ice-9 match)
  #:export (<compiler>
            <c-compiler>
            <meson>
            <env>
            <configuration-data>
            <external-program>
            <dependency>
            <file>
            <lib>
            <run-result>
            <feature>
            configuration.table
            .version
            .meson-version
            .variables
            .languages
            .license
            .options
            .program
            .fount
            .name))
(define-class <dictionarie> ()
  (tb #:init-form (make-hash-table) #:getter .tb))

(define (make-dictionarie . l)
  (let* ((d (make <dictionarie>))
         (tb (.tb d)))
    (for-each (match-lambda ((v b) (hash-set! tb v b)))l )
    d))

(define-class <compiler> ())
(define-class <c-compiler> (<compiler>))
(define-class <dependency> ()
  (name #:getter .name #:init-keyword #:name))
(define-class <build-target> ())
(define-class <lib> (<build-target>))
(define-class <feature> ())
(define-class <run-result> ())
(define-method (write (d <dependency>) port)
  (format port "#<dependency '~a' ~x>" (.name d) (object-address d) ))
(define (make-meson-default-optional-table)
  (define table(make-hash-table))
  (define (define! v b)
    (hash-set! table v b))
  (define! "prefix" "/usr")
  (define! "libdir" "/lib")
  (define! "infodir" "share/info")
  (define! "includedir" "include")
  (define! "sysconfdir" "/etc")
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
(define-class <file> ()
  (name #:init-keyword #:name #:getter .name))

(define-class <external-program> ()
  (program #:init-value #f #:init-keyword #:program #:accessor .program)
  (found? #:init-value #f #:accessor .fount))

(define-method (write (d <external-program>) port)
  (format port "#<<external-program> '~a' ~x>" (.program d) (object-address d) ))

(define-class <configuration-data> ()
  (readonly? #:init-value #f #:accessor conf-data-readonly?)
  (table #:init-thunk make-hash-table #:getter configuration.table))

(define-public %meson (make-parameter (make <meson>)))
