(define-module (meson compiler cpp)
  #:use-module (meson compiler)
  #:use-module (meson types)
  #:export (<cpp-compiler>))

(define-compiler <cpp-compiler> cpp)
