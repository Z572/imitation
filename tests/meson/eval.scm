(use-modules (language meson parse)
             (oop goops)
             (meson types)
             (system base compile)
             (ice-9 match))
(define-syntax-rule (rc str)
  (call-with-input-string
      (string-append str
                     "\n")
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
    (meson-languages
     (%meson)))
  (test-equal "meson-name"
    "if test"
    (meson-name
     (%meson))))
