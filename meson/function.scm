(define-module (meson function)
  #:use-module (meson types)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:re-export (pk error)
  #:export-syntax (assert))

(define-public (%vector . args)
  (apply vector args))
(define-public (%relational kw i o)
  (match kw
    ('(not in)
     (not (%relational '(in) i o)))
    ('(in)
     (->bool (member i (vector->list o))))))

(define*-public (project name
                         language
                         #:key
                         version
                         license
                         default_options
                         meson_version
                         )
  (pk '%meson (%meson))
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

(define*-public (get_option name)
  (pk 'get_option name))

(define-public (!= a b)
  (not (equal? a b)))
(define-public (== a b)
  (equal? a b))

(define-syntax define-method-public
  (lambda (x)
    (syntax-case x ()
      ((define-method-public (name . args) body ...)
       #`(begin
           (define-method #,(cons #'name #'args)
             body ...)
           (export name))))))

(define-method-public (set (o <configuration-data>) key value . args)
  (apply (lambda* (#:key (description #f))
           (hash-set! (configuration.table o) key
                      `((value . ,value)
                        ((description . ,description)))))
         args))
(define-method-public (set10 (o <configuration-data>) key value . args)
  (apply set o key (->bool value) args))

(define-method-public (get (o <configuration-data>) key . args)
  (assoc-ref (hash-ref (configuration.table o) key) 'value))

(define-method-public (startswith (o <string>) start)
  (string-prefix? start o))

(define-method (meson-/ v1 v2)
  (/ v1 v2))

(define-method-public (meson-/ (str1 <string>) (str2 <string>))
  (string-append str1 "/" str2))

(define-method-public (meson-% v1 v2)
  (remainder v1 v2))

(define-method-public (%subscript (vc <vector>) index)
  (vector-ref vc index))

(define-method-public (%subscript (vc <string>) index)
  (string-ref vc index))
