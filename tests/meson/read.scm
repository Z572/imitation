(use-modules (language meson parse)
             (language tree-il)
             (ice-9 match))

(define (read* str)
  (call-with-input-string
      (string-append str "\n")
    (lambda (x)
      (tree-il->scheme
       (meson-ast->tree-il
        (read-meson x (current-module)))))))

(define-syntax-rule (check name body)
  (test-equal name
    (read* name)
    'body))

(check "abc=1" ((@ (meson function) %assignment) 'abc 1))
(check "abc-=1" ((@ (meson function) %assignment-=) 'abc 1))
(check "abc+=1" ((@ (meson function) %assignment+=) 'abc 1))
(check "call()"
       ((@ (meson function) meson-call)
        'call
        ((@ (guile) list)) ((@ (guile) list))))
(check "call_with_arg('arg1')"
       ((@ (meson function) meson-call)
        'call_with_arg
        ((@ (guile) list) "arg1") ((@ (guile) list))))
(check "1+1"
       ((@ (meson function) +) 1 1))
(check "-1"
       ((@ (meson function) -) 1))
(check "+1"
       ((@ (meson function) +) 1))
(check "not true"
       ((@ (meson function) not) #t))

(check "call_with_karg('arg1',karg1:'20')"
       ((@ (meson function) meson-call)
        'call_with_karg
        ((@ (guile) list) "arg1")
        ((@ (guile) list) #:karg1 "20")))

(check "id" ((@ (meson function) %get-id) 'id))
(check "'string'" "string")
(check "true" #t)
(check "false" #f)
(check "#comment" (if #f #f))
(check "[]" ((@ (guile) list)))
(check "[1,2]"
       ((@ (guile) list) 1 2))
(check "[1,]" ((@ (guile) list) 1))

(check "{}"
       ((@@ (meson types) make-dictionarie)
        ((@ (guile) list))
        ((@ (guile) list))))
(check "{'a':20}"
       ((@@ (meson types) make-dictionarie)
        ((@ (guile) list) "a")
        ((@ (guile) list) 20)))

(check "2 in [2]"
       ((@ (meson function) meson-call)
        (quote %relational)
        ((@ (guile) list) (quote in) 2 ((@ (guile) list) 2))
        ((@ (guile) list))))
