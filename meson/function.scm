(define-module (meson function)
  #:use-module (meson types)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:re-export (pk)
  #:export-syntax (assert))
(define-public (%vector . args)
  (apply vector args))
(define-public (%relational kw i o)
  (match kw
    ('(not in)
     (not (%relational '(in) i o)))
    ('(in)
     (->bool (member i (vector->list o))))))
(define-public (%subscript vc index)
  (vector-ref vc index))

(define*-public (project name
                         language
                         #:key
                         version
                         license
                         default_options
                         meson_version
                         )
  (pk 'p name language version license default_options meson_version))

(define* (assert exp #:optional message)
  (pk 'assert exp message))

(define*-public (message arg)
  (pk 'message arg))

(define*-public (configuration_data #:optional dict)
  (make <configuration-data>))
(define*-public (environment o #:key (method "set") (separator ":"))
  (make <env>))

(define*-public (find_program path #:key (required #t) (default_optinos '()))
  (pk 'find_program required)
  (search-path (cons "." (parse-path (getenv "PATH"))) path))

(define-public (!= a b)
  (not (equal? a b)))
(define-public (== a b)
  (equal? a b))
