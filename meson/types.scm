(define-module (meson types)
  #:use-module (imitation utils)
  #:use-module (guix sets)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (srfi srfi-1)
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
            <meson-module>
            <option>
            <build-target>
            <custom-target>
            <host-machine>
            <build-machine>
            <target-machine>
            <dictionarie>
            <exe>
            <range>
            configuration.table
            .version
            .meson-version
            .variables
            .languages
            .license
            .options
            .program
            .fount
            .root
            .type
            .value
            .module
            .name
            .dependencys
            meson-targets
            meson-external-program
            .targets)
  #:export (define-class*))

(define-syntax-rule (define-class* a ...)
  (define-class a ...
    #:metaclass <redefinable-class>))

(define-class* <dictionarie> ()
  (tb #:init-form (make-hash-table) #:getter .tb))

(define (make-dictionarie k v)
  (let* ((d (make <dictionarie>))
         (tb (.tb d)))
    (for-each (lambda (k v) (hash-set! tb k v)) k v)
    d))
(define*-public (dictionarie-get dictionarie key #:optional fallback)
  (hash-ref (.tb dictionarie) key fallback))
(define*-public (dictionarie-has-key dictionarie key)
  (not (equal? 'not-found
               (dictionarie-get dictionarie key 'not-found))))
(define*-public (dictionarie-keys dictionarie)
  (hash-map->list (lambda (k v) k) (.tb dictionarie)))
(define*-public (dictionarie-for-each proc dictionarie)
  (hash-for-each proc (.tb dictionarie)))

(define-class* <compiler> ())
(define-class* <dependency> ()
  (name #:getter .name #:init-keyword #:name #:init-value #f))

(define-class* <meson-module> ()
  (module #:init-keyword #:module #:getter .module))
(define-method (write (d <meson-module>) port)
  (format port "#<<meson-module> ~a ~x>" (or (module-name (.module d)) "unknown-name!") (object-address d) ))
(define-class* <meson-target> ()
  (name #:init-keyword #:name #:getter .name #:init-value #f))

(define-method (initialize (m <meson-target>) args)
  (let ((o (next-method))
        (meson (%meson)))
    (set! (.targets meson)
          (set-insert m (.targets meson)))
    o)
  )
(define-method (write (d <meson-target>) port)
  (format port "#<~a ~a ~x>" (class-name(class-of d))
          (or (.name d) "unknown-name!") (object-address d) ))
(define (meson-targets m)
  (set->list (.targets m)))
(define-class* <alias-target> (<meson-target>))
(define-class* <build-target> (<meson-target>)
  (dependencys #:init-keyword #:dependencys #:getter .dependencys))

(define-class* <custom-target> (<meson-target>))
(define-class* <run-target> (<meson-target>))

(define-class* <range> ()
  (start #:init-keyword #:start #:init-value 0)
  (stop #:init-keyword #:stop)
  (step #:init-keyword #:step #:init-value 1))

(define-class* <lib> (<build-target>))
(define-class* <exe> (<build-target>))
(define-class* <feature> ())
(define-class* <run-result> ())
(define-method (write (d <dependency>) port)
  (format port "#<<dependency> '~a' ~x>" (or (.name d) "unknown-name!") (object-address d) ))

(define (make-meson-default-optional-table)
  (define table(make-hash-table))
  (define (define! v b)
    (hash-set! table v (make <option> #:name v #:value b #:type "string")))
  (define! "prefix" "/usr")
  (define! "bindir" "/bin")
  (define! "sbindir" "/sbin")
  (define! "datadir" "/share")
  (define! "libdir" "/lib")
  (define! "infodir" "share/info")
  (define! "includedir" "include")
  (define! "sysconfdir" "/etc")
  (define! "buildtype" "debug")
  (define! "c_args" (list))
  table)

(define-class* <option> ()
  (name #:init-keyword #:name #:getter .name)
  (value #:init-keyword #:value  #:getter .value)
  (type #:init-keyword #:type #:getter .type)
  (description #:init-keyword #:description)
  )

(define-class* <build-machine> ())
(define-class* <host-machine> (<build-machine>))
(define-class* <target-machine> (<build-machine>))

(define-method (write (d <option>) port)
  (format port "#<<option> '~a' type: '~a' value: '~S'~x>"
          (.name d)
          (.type d)
          (.value d)
          (object-address d) ))
(define-class* <meson> ()
  (name #:init-keyword #:name #:accessor .name)
  (version #:accessor .version #:init-value #f)
  (license #:accessor .license #:init-value #f)
  (meson-version #:accessor .meson-version #:init-value "1.1.1")
  (options #:accessor .options #:init-form (make-meson-default-optional-table))
  (variables #:getter .variables #:init-form (make-module))
  (languages #:accessor .languages)
  (root #:accessor .root #:init-value #f)
  (build-machine #:getter .build-machine #:init-thunk (lambda () (make <build-machine>)))
  (host-machine #:getter .host-machine #:init-thunk (lambda () (make <host-machine>)))
  (target-machine #:getter .target-machine #:init-thunk (lambda () (make <target-machine>)))
  (targets #:accessor .targets #:init-value (list->set '()))
  (external-program #:accessor .external-program #:init-value (list->set '())))

(define-public (meson-name m)
  (.name m))
(define-public (meson-languages m)
  (set->list (.languages m)))
(define-method (initialize (m <meson>) opt)
  (let* ((o (next-method))
         (variables (.variables o)))
    (module-define! variables 'meson m)
    (module-define! variables 'build_machine (.build-machine o))
    (module-define! variables 'host_machine (.host-machine o))
    (module-define! variables 'taregt_machine (.target-machine o))
    o))
(define-class* <env> ())
(define-class* <file> ()
  (name #:init-keyword #:name #:getter .name))

(define-method (write (d <file>) port)
  (format port "#<<file> '~a' ~x>" (.name d) (object-address d) ))

(define-class* <external-program> ()
  (program #:init-value #f #:init-keyword #:program #:accessor .program)
  (origin-path #:init-value #f #:init-keyword #:origin-path #:accessor .origin-path)
  (found? #:init-value #f #:accessor .fount))

(define-method (initialize (m <external-program>) args)
  (let ((o (next-method))
        (meson (%meson)))
    (set! (.external-program meson)
          (set-insert m (.external-program meson)))
    o)
  )

(define (meson-external-program m)
  (set->list (.external-program m)))

(define-method (write (d <external-program>) port)
  (format port "#<<external-program> '~a' found: '~a' ~x>"
          (.origin-path d)
          (.program d) (object-address d) ))

(define-class* <configuration-data> ()
  (readonly? #:init-value #f #:accessor conf-data-readonly?)
  (table #:init-thunk make-hash-table #:getter configuration.table))

(define-once %meson
  (make-parameter (make <meson>)))
(export %meson)
(define-once %meson-current-directory
  (make-parameter #f))
(export %meson-current-directory)
