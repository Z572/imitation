(use-modules (language meson parse)
             (oop goops)
             (meson types)
             (system base compile)
             (ice-9 match))
(define-syntax-rule (rc str ...)
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
      (%meson))))

(parameterize ((%meson (make <meson>)))
  (rc "project('if test', 'c')")
  (test-equal "language"
    '("c")
    (meson-languages))
  (test-equal "meson-name"
    "if test"
    (meson-name)))

(define-syntax-rule (with-new-meson body ...)
  (parameterize ((%meson (make <meson>)))
    body ...))

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
     "dict={}")
 (check-variable "number" a 20)
 (check-variable "bool1" b #t)
 (check-variable "bool2" b2 #f)
 (check-variable "array" arr '()))

(with-new-meson
 (rc "a=true.to_int()"
     "b=false.to_int()")
 (check-variable a 1)
 (check-variable b 0))
