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
     "dict={}")
 (check-variable "number" a 20)
 (check-variable "bool1" b #t)
 (check-variable "bool2" b2 #f)
 (check-variable "array" arr '()))

(with-new-meson
 (rc "a=true.to_int()"
     "b=false.to_int()"
     "c=true.to_string('#t')"
     "d=false.to_string('#f')")
 (check-variable a 1)
 (check-variable b 0)
 (check-variable c "#t")
 (check-variable d "false"))


(with-new-meson
 (rc "endswith='endswith'.endswith('with')"
     "noendswith='endswith'.endswith('nowith')"
     "to_lower='TO_LOWER'.to_lower()"
     "to_upper='to_upper'.to_upper()")
 (check-variable "endswith" endswith #t)
 (check-variable noendswith #f)
 (check-variable to_lower "to_lower")
 (check-variable to_upper "TO_UPPER"))
