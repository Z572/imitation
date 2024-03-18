(define-module (meson function)
  #:use-module (system base target)
  #:use-module (meson types)
  #:use-module (guix sets)
  #:use-module (system base compile)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (ice-9 match)
  #:use-module (rnrs base)
  #:use-module ((guix licenses) #:prefix license:)
  #:re-export (pk)
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
  #:export ((%assert . assert)
            (meson-append . append)
            (meson-prepend . prepend)
            (meson-/ . /)
            (meson-% . %)
            (meson-+ . +)
            (meson-- . -)
            (meson-format . format)
            (meson-error . error)
            (meson-system . system)
            (meson-cpu . cpu)))

(define-public (%relational kw i o)
  (match kw
    ('(not in)
     (not (%relational '(in) i o)))
    ('(in)
     (->bool (member i (pk '%relational o))))))

(define (current-module*)
  (resolve-interface '(meson function)))

(define*-public (meson-method-call object func . args)
  (pk 'method-call object func args)
  (define is-module (is-a? object <meson-module>))
  (let ((func* (module-ref (if is-module
                               (.module object)
                               (current-module*))
                           func #f)))
    (if func*
        (apply func* (append (if is-module '() (list object)) args))
        (error 'no-method-found (format #f "no method call '~a'" func)))))

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
    (when=> meson_version .meson-version)
    (when=> (list->set (ensure-list language)) .languages)
    (when=> version .version)
    (when=>  (license-case license) .license)
    (when=> (%meson-current-directory) .root)
    (when (and (.root meson)
               (file-exists? (string-append (.root meson) "/" "meson_options.txt")))
      (compile-and-load
       (string-append (.root meson) "/" "meson_options.txt")
       #:from (@ (language meson spec) meson)
       #:to 'value
       #:env (current-module))))
  (when default_options
    (let ((op(.options meson)))
      (for-each  (lambda (x)
                   (match (string-split x #\=)
                     ((name value )
                      (hash-set! op name (make <option> #:name name #:value value #:type "string")))))
                 default_options)))
  (pk 'p name language version license default_options meson_version))

(define*-public (option name #:key
                        type
                        description
                        deprecated
                        choices
                        (value
                         (match type
                           ("string" "")
                           ("feature" (make <feature>))
                           ("combo" (list-ref choices 0))
                           ("boolean" #t)
                           ("integer" 0)
                           (erro (error 'not-knoew! "t" erro)))))
  (let ((op (.options (%meson))))
    (hash-set! op name (make <option>
                         #:name name
                         #:value value
                         #:type type
                         #:description description))))

(define* (%assert exp #:optional message)
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
  (pk 'message arg)
  (format #t "message: ~a~%" arg))

(define*-public (configuration_data #:optional dict)
  (make <configuration-data>))

(define*-public (environment #:optional o #:key (method "set") (separator ":"))
  (make <env>))
(define*-public (dependency name
                            #:key
                            allow_fallback
                            default_options
                            disabler
                            fallback
                            (include_type "preserve")
                            language
                            (method "auto")
                            (native #f)
                            not_found_message
                            (required #t)
                            (static #f)
                            version)
  (make <dependency> #:name name))
(define*-public (include_directories a . o)
  (pk 'include_directories))

(define*-public (library a
                  source
                  #:key
                  (build_by_default #t)
                  (dependencies '())
                  extra_files
                  gnu_symbol_visibility
                  gui_app
                  implicit_include_directories
                  include_directories
                  install
                  install_dir
                  install_mode
                  install_rpath
                  install_tag
                  link_args
                  link_depends
                  link_language
                  link_whole
                  link_with
                  name_prefix
                  name_suffix
                  (native #f)
                  objects
                  override_options
                  rust_abi
                  rust_crate_type
                  rust_dependency_map
                  sources
                  soversion
                  version
                  #:allow-other-keys)
  (pk 'library a source 'dependencies dependencies)
  (make <lib> #:dependencies dependencies))

(define*-public (shared_library a
                                source
                                #:key
                                (build_by_default #t)
                                (dependencies '())
                                extra_files
                                gnu_symbol_visibility
                                gui_app
                                implicit_include_directories
                                include_directories
                                install
                                install_dir
                                install_mode
                                install_rpath
                                install_tag
                                link_args
                                link_depends
                                link_language
                                link_whole
                                link_with
                                name_prefix
                                name_suffix
                                (native #f)
                                objects
                                override_options
                                rust_abi
                                rust_crate_type
                                rust_dependency_map
                                sources
                                soversion
                                version
                                #:allow-other-keys)
  (pk 'shared_library a source 'dependencies dependencies)
  (make <lib>))

(define*-public (executable a
                            #:key
                            (link_with '())
                            (install #f)
                            (build_by_default #t)
                            (dependencies '())
                            (gnu_symbol_visibility #f)
                            (native #f)
                            #:rest o)
  (pk 'executable a link_with 'install install 'dependencies dependencies 'rest o)
  (make <exe> #:name a #:dependencys dependencies))

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
  (pk 'import)
  (let ((m (resolve-interface `(meson module ,(string->symbol a)))))
    (make <meson-module>
      #:module m)))
(define*-public (generate a . o)
  (pk 'generate)  )

(define*-public (files . o)
  (map (lambda (x) (make <file> #:name x))
       o))

(define*-public (find_program path #:key (required #t) (default_optinos '()))
  (pk 'find_program required)
  (make <external-program>
    #:program
    (search-path (cons "." (parse-path (getenv "PATH"))) path)))

(define*-public (get_option name)
  (assert (string? name))
  (let ((v (hash-ref (.options (%meson)) name #f)))
    (if v
        (match (.type v)
          ("string" (.value v))
          ("feature" (make <feature>))
          ("combo" (.value v))
          ("boolean" (.value v))
          ("integer" (.value v))
          (erro (error 'not-knoew! "t" erro)))
        (error 'not-found! "I want to get ~a,but not option found" name))
    ))

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


(define-method-public (cmd_array (compiler <compiler>))
  (pk 'compiler 'cmd_array compiler)
  'cmd_array)
(define*-public (get_compiler meson language #:key (native #f))
  (assert (is-a? meson <meson>))
  1
  (make <compiler>))
(define*-public (project_version meson)
  (assert (is-a? meson <meson>))
  (.version meson))

(define-method-public (set (o <env>) key value . args)
  (pk 'set-env))

(define-method (meson-cpu (o <build-machine>))
  (target-cpu))

(define-method-public (cpu_family (o <build-machine>))
  "amd64")

(define cpu-endianness (delay (@@ (system base target) cpu-endianness)))
(define-method-public (endian (o <build-machine>))
  (let ((cpu (meson-cpu o))
        (f (force cpu-endianness)))
    (symbol->string
     (f (match cpu
          ("amd64" "x86_64")
          (else cpu))))))

(define-method (meson-system (o <build-machine>))
  "system")

(define-method-public (set (o <configuration-data>) key value . args)
  (apply (lambda* (#:key (description #f))
           (hash-set! (configuration.table o) key
                      `((value . ,value)
                        ((description . ,description)))))
         args))
(define-method-public (set10 (o <configuration-data>) key value . args)
  (apply set o key (->bool value) args))

(define-method-public (set_quoted (o <configuration-data>) key value . args)
  (pk 'set_quoted)
  ;; (apply set o key (->bool value) args)
  )

(define-method-public (get (o <configuration-data>) key . args)
  (assoc-ref (hash-ref (configuration.table o) key) 'value))

(define-method (get (o <dictionarie>) key fallback)
  (dictionarie-get o key fallback))
(define-method (get (o <dictionarie>) key)
  (dictionarie-get o key))
(define-method-public (keys (o <dictionarie>))
  (dictionarie-keys o))

(define-method-public (has_key (o <dictionarie>) str)
  (dictionarie-has-key o str))

(define-method-public (startswith (o <string>) start)
  (string-prefix? start o))

(define-method-public (split (o <string>) (char <string>))
  (string-split  o (string-ref char 0)))
(define-method-public (split (o <file>) (char <string>))
  (split (.name o) char))

(define-method-public (split (o <list>) (char <string>))
  (map (lambda (s) (split s char))
       o))

(define-method-public (project_license (o <meson>))
  (.license o))

(define*-public (underscorify str)
  (pk 'underscorify str ))
(define*-public (to_upper str)
  (pk 'to_upper (string-upcase str )))
(define-public (%assignment name value)
  (let ((hm (.variables (%meson))))
    (if #t;; (module-defined? hm name)
        ;; (error 'redefine! "")
        (module-define! hm name value))))

(define-public (%assignment+= name value)
  (let ((hm (.variables (%meson))))
    (if (module-defined? hm name)
        (module-define! hm name (meson-+ (module-ref hm name) value))
        (error '%assignment-no-defined! "+="))))

(define* (meson-append env a b)
  (pk 'meson-append))

(define* (meson-prepend env a b)
  (pk 'meson-prepend))

(define-public (%get-id name)
  (module-ref (.variables (%meson)) name ))

(define-method (meson-/ (v1 <number>) (v2 <number>))
  (/ v1 v2))



(define-method-public (meson-/ (str1 <string>) (str2 <string>))
  (string-append str1 "/" str2))


(define-method-public (meson-- (n1 <number>))
  (- n1))

(define-method-public (meson-- (n1 <number>) (n2 <number>))
  (- n1 n2))

(define-method-public (meson-+ (n1 <number>) (n2 <number>))
  (+ n1 n2))

(define-method-public (meson-+ (str1 <string>) (str2 <string>))
  (string-append str1  str2))

(define-method-public (meson-+ (str1 <list>) s)
  (append str1 (list s)))

(define-method-public (meson-error str1)
  (error 'meson-error str1))
(define-method-public (meson-% v1 v2)
  (remainder v1 v2))

(define-method-public (join (str <string>) a)
  (pk 'join)
  str)

(define*-public (find_library str name #:key (required #t))
  (assert (is-a? str <compiler>))
  (pk 'find_library)
  (make <dependency> #:name name))

(define-method-public (found (f <external-program>))
  (pk 'found <external-program>)
  #t)

(define-method-public (found (f <dependency>))
  (pk 'found-dep f)
  #t)

(define-method-public (%subscript (vc <list>) index)
  (list-ref vc index))

(define-method-public (%subscript (vc <string>) index)
  (string-ref vc index))

(define-method-public (strip (vc <string>))
  (pk 'strip)
  "")
(define-method-public (strip (vc <string>) strip_chars)
  (pk 'strip)
  "")

(define-method-public (auto (vc <feature>))
  (pk 'auto)
  "auto"
  #t)
(define-method-public (current_source_dir (m <meson>))
  (pk 'current_source_dir))
(define-method-public (current_build_dir (m <meson>))
  (pk 'current_source_dir))

(define*-public (meson-format s . arg)
  (pk s 'meson-format "meson-format"))

(define*-public (run_command s . arg)
  (pk  "run_command"
       (make <run-result>)))
(define-method-public (stdout ( r <run-result>))
  (pk 'stdout)
  "stdout")

(define-method-public (stderr ( r <run-result>))
  'stderr)

(define-method-public (returncode ( r <run-result>))
  (pk 'returncode)
  0)
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
    (pk 'subdir dir)
    (call-with-prompt 'subdir_done
      (lambda ()
        (compile-and-load
         "meson.build"
         #:from (@ (language meson spec) meson)
         #:to 'value
         #:env (current-module)))
      (lambda (k) k))))

(define-public (subdir_done)
  ((abort-to-prompt 'subdir_done)))
