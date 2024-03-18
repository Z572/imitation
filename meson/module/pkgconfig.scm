(define-module (meson module pkgconfig)
  #:use-module (oop goops)
  #:use-module (meson types)
  #:export (generate))

(define* (generate . rest)
  (pk 'pkgconfig-generate rest))
