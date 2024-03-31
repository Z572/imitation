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

(parameterize ((%meson (make <meson>)))
  (rc "a=20"
      "b=true"
      "b2=false"
      "arr=[]"
      "dict={}")
  (let-syntax ((check
                (syntax-rules ()
                  ((_ n expect varname)
                   (test-equal n
                     expect
                     (variable-ref
                      (car (assoc-ref (meson-variables)
                                      varname))))))))
    (check "number" 20 'a)
    (check "bool1" #t 'b)
    (check "bool2" #f 'b2)
    (check "array" '() 'arr)))
