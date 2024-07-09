(use-modules (language meson parse)
             (system base target)
             (oop goops)
             (meson types)
             (system base compile)
             (ice-9 match))
(define-syntax-rule (check-exp str o)
  (test-equal str o
              (compile (read-meson str)
                       #:from 'meson
                       #:to 'value)))
(define-syntax-rule (rc str ...)
  (false-if-exception
   (call-with-input-string
       (string-join (list str ...) "\n" 'suffix)
     (lambda (x)
       (call-with-prompt 'subdir_done
         (lambda ()
           (read-and-compile
            x
            #:from 'meson
            #:to 'value))
         (lambda (k) k))
       (%meson)))))

(define-syntax-rule (with-new-meson body ...)
  (parameterize ((%meson (make <meson>)))
    body ...))

(with-new-meson
 (rc "project('if test', 'c')")
 (test-equal "language"
   '("c")
   (meson-languages))
 (test-equal "meson-name"
   "if test"
   (meson-name)))

(define-syntax check-variable
  (syntax-rules ()
    ((_ n varname expect)
     (test-equal n
       expect
       (variable-ref
        (car (assoc-ref (meson-variables)
                        'varname)))))
    ((_ expect varname)
     (check-variable (object->string 'varname)
                     expect varname))))
(with-new-meson
 (rc "a=20"
     "b=true"
     "b2=false"
     "arr=[]"
     "dict={}"
     "two=1+1"
     "zero=1-1"
     "onezero=-1"
     "not_true=not true"
     "is_in=20 in [20,30]"
     "mes='a'+ 'b'")
 (check-variable a 20)
 (check-variable b #t)
 (check-variable b2 #f)
 (check-variable arr '())
 (check-variable two 2)
 (check-variable onezero -1)
 (check-variable not_true #f)
 (check-variable is_in #t)
 (check-variable mes "ab"))
(with-new-meson
 (rc "a=1"
     "a+=1")
 )

(check-exp "c=1 / 2
c" 0)
(check-exp "c='hello' / 'world'
c" "hello/world")
(test-expect-fail 1)
(check-exp"'hello' / '/world'" "/world")
(check-exp "false.to_int()" 0 )
(check-exp "true.to_int()" 1)
(check-exp "false.to_string()" "false")
(check-exp "true.to_string()" "true" )
(check-exp "true.to_string('#t','#f')" "#t" )

(check-exp "'endswith'.endswith('with')" #t)
(check-exp "'endswith'.endswith('nowith')" #f)
(check-exp "'TO_LOWER'.to_lower()" "to_lower")
(check-exp "'to_upper'.to_upper()" "TO_UPPER")
(check-exp "'abc def -'.underscorify()" "abc_def__")
(check-exp "build_machine.endian()" "little")
(check-exp "executable('f', 'f.c', install : true).found()" #t)
(check-exp "executable('f', 'f.c', install : true).name()"  "f")
