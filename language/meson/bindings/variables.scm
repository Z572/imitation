(define-module (language meson bindings variables)
  #:use-module ((guile)
                #:select
                (
                 >
                 <
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
                 getenv
                 eval-when))
  )
