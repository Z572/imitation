(define-module (meson module fs)
  #:use-module (oop goops)
  #:use-module (meson types)
  #:export (exists
            is_absolute))

(define-method (exists str)
  (file-exists? str))

(define-method (is_absolute str)
  (absolute-file-name? str))
