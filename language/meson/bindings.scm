(define-module (language meson bindings)
  #:pure
  #:use-module ((rnrs base) #:select ((assert . %rnrs-assert)))
  #:use-module (srfi srfi-34)
  #:use-module ((srfi srfi-1) #:prefix srfi-1:)
  #:use-module ((rnrs conditions) #:select (message-condition? condition make-message-condition
                                                               make-assertion-violation
                                                               assertion-violation?))
  #:use-module ((oop goops) #:select (<number>
                                      <string>
                                      <vector>
                                      is-a?
                                      make
                                      define-method
                                      define-class))
  #:use-module ((guile)
                #:select
                (include-from-path
                 >
                 <
                 +
                 define-syntax-rule
                 or
                 quote
                 eq?
                 equal?
                 quasiquote
                 set!
                 module-define!
                 module-ref
                 current-module
                 if
                 list
                 syntax-case syntax quasisyntax unsyntax unsyntax-splicing
                 syntax->datum datum->syntax identifier?
                 generate-temporaries free-identifier=? bound-identifier=?
                 with-syntax identifier-syntax
                 lambda
                 lambda* define*

                 call-with-prompt abort-to-prompt
                 ash logand logior logxor lognot logtest logbit?
                 keyword?
                 bitvector?
                 cons*
                 fluid-ref fluid-set! with-fluid* with-dynamic-state
                 make-variable variable-ref variable-set!
                 keyword->symbol symbol->keyword
                 exact->inexact
                 inf? nan?
                 error
                 string->symbol
                 raise-exception
                 define-syntax
                 pk
                 begin
                 *unspecified*
                 false-if-exception
                 module-variable
                 resolve-module
                 and
                 ->bool
                 member
                 not
                 error
                 let
                 make-module
                 apply
                 string->number
                 number->string
                 set-module-name!
                 define-public
                 string
                 string-ref
                 vector-ref
                 vector
                 vector->list
                 string=?
                 resolve-interface
                 getenv
                 eval-when))
  #:re-export (module-define!
               equal?
               and
               error
               apply
               module-ref
               ->bool
               >
               <
               not
               member
               begin
               list
               quote
               vector->list
               *unspecified*
               (vector . %vector))
  #:export (%module %var-module project message to_int to_string %subscript set
                    assert
                    get_compiler
                    %call
                    %mcall
                    %assignment
                    %id
                    configuration_data
                    %equal
                    %relational
                    contains
                    environment
                    project_version))

(define* %var-module (resolve-module '(language meson bindings variables))
  ;; (make-module)
  )
;; (set-module-name! %var-module '(language meson bindings variables))
(define* (message arg)
  (pk 'message arg))

(define* (project name
                  language
                  #:key
                  version
                  license
                  default_options
                  meson_version
                  )
  (pk 'p name language version license default_options meson_version))

(define-method (contains (o <vector>) i)
  (member i (vector->list o)))

(define-class <configuration-data> ()
  (readonly? #:init-value #f #:accessor conf-data-readonly?))
(define-class <compiler> ())
(define-class <c-compiler> (<compiler>))
(define-class <meson> ())
(define-class <env> ())
(define* (environment o #:key (method "set") (separator ":"))
  (make <env>))
(define* current-meson (make <meson>))
(module-define! %var-module 'meson current-meson)
(define-method (project_version (m <meson>) )
  (pk 'project_version m ))
(define-method (get_compiler (m <meson>) l )
  (if (string=? l "c")
      (make <c-compiler>)
      ;; (or (getenv "CC") "gcc")
      (make <compiler>)))
(define* (configuration_data #:optional dict)
  (make <configuration-data>))

(define* (set cd tag value #:key description #:allow-other-keys)
  (assert (is-a? cd <configuration-data>))
  (assert (not (conf-data-readonly? cd)))
  (pk 'set))

(define* (set10 cd tag value #:key description #:allow-other-keys)
  (assert (is-a? cd <configuration-data>))
  (assert (not (conf-data-readonly? cd)))
  (pk 'set10 (->bool value)))

(define-method (%subscript (o <string>) index)
  (string (string-ref o index)))
(define-method (%subscript (o <vector>) index)
  (vector-ref o index))
(define-method (to_int (str <string>))
  (string->number str))

(define-method (to_string (n <number>))
  (number->string n))


(define-syntax-rule (%id o)
  (module-ref %var-module 'o))

(define-syntax assert
  (lambda (x)
    (syntax-case x ()
      ((_ e) #`(%rnrs-assert e))
      ((_ expression n)
       #`(or expression
             (raise (condition
                     (make-assertion-violation)
                     (make-message-condition
                      n))))))))

(define-syntax %call
  (lambda (x)
    (syntax-case x ()
      ((_ func)
       #`(func))
      ((_ . func)
       #`func))))

(define-syntax %mcall
  (lambda (x)
    (pk x)
    (syntax-case x ()
      ((_ (obj func))
       #`(func obj ))
      ((_ (obj func) arg args ...)
       #`(func obj arg args ...))
      )))
(define-syntax %equal
  (lambda (x)
    (syntax-case x (== !=)
      ((_ == v1 v2)
       #`(equal? v1 v2))
      ((_ != v1 v2)
       #`(not (equal? v1 v2))))))

(define-syntax %relational
  (lambda (x)
    (syntax-case x (in not)
      ((_ (not in) v lst)
       #`(not (%relational (in) v lst)))
      ((_ (in) v lst)
       #`(->bool (member v (vector->list lst))))
      ((_ (op) v v2)
       #`(->bool (op v v2))))))

(define-syntax %assignment
  (lambda (x)
    (syntax-case x (= +=)
      ((_ = name value)
       #`(let ((v value))
           (if (false-if-exception
                (module-ref %var-module 'name))
               (error 'name 'redefine-to  'value)
               (module-define! %var-module 'name v))))
      ((_ += name value)
       #`(let ((v value)
               (exists (false-if-exception
                        (module-ref %var-module 'name))))
           (if exists
               (module-define! %var-module 'name
                               (+ exists v))
               (error '+= 'name 'no-exists)
               ))))))
