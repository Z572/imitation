(define-module (meson function)
  #:use-module (meson types)
  #:use-module (guix sets)
  #:use-module (system base compile)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops describe)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:re-export (pk error)
  ;; #:pure
  ;; #:use-module ((guile) #:select (define-public
  ;;                                  error
  ;;                                  syntax-case
  ;;                                  lambda
  ;;                                  define-syntax
  ;;                                  pk
  ;;                                  equal?
  ;;                                  syntax
  ;;                                  quasisyntax
  ;;                                  string-prefix?
  ;;                                  remainder
  ;;                                  /
  ;;                                  string-append
  ;;                                  unsyntax
  ;;                                  ...
  ;;                                  cons
  ;;                                  begin
  ;;                                  export
  ;;                                  getenv
  ;;                                  vector->list
  ;;                                  vector-ref
  ;;                                  member
  ;;                                  not
  ;;                                  apply
  ;;                                  ->bool
  ;;                                  hash-ref
  ;;                                  assoc-ref
  ;;                                  vector
  ;;                                  quasiquote
  ;;                                  hash-set!
  ;;                                  parse-path
  ;;                                  search-path
  ;;                                  quote))
  #:export-syntax (assert))

(define-public (%vector . args)
  (apply vector args))
(define-public (%relational kw i o)
  (match kw
    ('(not in)
     (not (%relational '(in) i o)))
    ('(in)
     (->bool (member i (vector->list o))))))

(define (license-case str)
  (match str
    ("LGPLv2+" license:lgpl2.0+)
    (else (pk 'unknow-license!)
          str)))
(define (ensure-list o)
  (if (list? o)
      o
      (list o)))
(define*-public (project name
                         language
                         #:key
                         version
                         (license #f)
                         (default_options #())
                         meson_version
                         )
  (define meson (%meson))
  (let-syntax ((when=>
                (syntax-rules ()
                  ((_ var acc)
                   (let ((v var))
                     (when v
                       (set! (acc meson) v)))))))
    (pk '%meson (%meson))
    (when=> meson_version .meson-version)
    (when=> (list->set (ensure-list language)) .languages)
    (when=> version .version)
    (when=>  (license-case license) .license)


    (when default_options
      (let ((op(.options meson)))
        (vector-for-each (lambda (n x)
                           (match (string-split x #\=)
                             ((name value ) (hash-set! op name value))))
                         default_options)))
    )

  (pk 'p name language version license default_options meson_version))

(define* (assert exp #:optional message)
  (pk 'assert exp message))

(define*-public (summary key_or_dict
                         #:optional
                         value
                         #:key (bool_yn #f)
                         list_sep
                         (section #f))
  (pk 'summary
      section
      ))

(define*-public (message arg)
  (pk 'message arg))

(define*-public (configuration_data #:optional dict)
  (make <configuration-data>))

(define*-public (environment o #:key (method "set") (separator ":"))
  (make <env>))
(define*-public (dependency name)
  (make <dependency> #:name name))
(define*-public (include_directories a . o)
  (pk 'include_directories))
(define*-public (shared_library a . o)
  (pk 'shared_library))
(define*-public (executable a
                            #:key
                            (link_with '())
                            (install #f)
                            (build_by_default #t)
                            (dependencies '())
                            (gnu_symbol_visibility #f)
                            (native #f)
                            #:rest o)
  (pk 'executable a link_with 'install install 'rest o))

(define*-public (add_languages lang
                               #:key
                               (native #t)
                               (required #t))
  (set! (.languages(%meson) )
        (set-insert lang (.languages(%meson) )))
  (pk 'add_languages
      (.languages(%meson) )
      lang native required))

(define*-public (test a . o)
  (pk 'test))
(define*-public (declare_dependency a . o)
  (pk 'declare_dependency)  )
(define*-public (install_headers a . o)
  (pk 'install_headers)  )
(define*-public (import a . o)
  (pk 'import)  )
(define*-public (generate a . o)
  (pk 'generate)  )

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

(define-method-public (get_compiler (o <meson>))
  1)

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

(define-method-public (project_license (o <meson>))
  (.license o))

(define-method (meson-/ v1 v2)
  (/ v1 v2))

(define-public (%assignment name value)
  (let ((hm (.variables (%meson))))
    (pk '%assignment name value)
    (if (module-defined? hm name)
        (error 'redefine!)
        (module-define! hm name value))))

(define-public (%assignment+= name value)
  (let ((hm (.variables (%meson))))
    (if (module-defined? hm name)
        (module-define! hm name (meson-+ (module-ref name) value))
        (error '%assignment-no-defined!))))

(define-public (%get-id name)
  (module-ref (.variables (%meson)) name ))

(define-method-public (meson-/ (str1 <string>) (str2 <string>))
  (string-append str1 "/" str2))

(define-method-public (meson-+ (str1 <string>) (str2 <string>))
  (string-append str1  str2))

(define-method-public (meson-% v1 v2)
  (remainder v1 v2))

(define-method-public (%subscript (vc <vector>) index)
  (vector-ref vc index))

(define-method-public (%subscript (vc <string>) index)
  (string-ref vc index))

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

(define-public (subdir dir)
  (with-directory-excursion dir
    (compile-and-load
     "meson.build"
     #:from (@ (language meson spec) meson)
     #:to 'value
     #:env (current-module))))
