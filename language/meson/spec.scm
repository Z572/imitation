(define-module (language meson spec)
  #:use-module (ice-9 threads)
  #:use-module (ts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors gnu)
  #:use-module (ice-9 textual-ports)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:use-module (system base target)
  #:use-module (ice-9 match)
  #:use-module (language meson parse)
  #:export (meson))

(define-language meson
  #:title "meson build system"
  #:reader read-meson
  #:parser parse-meson
  #:make-default-environment
  (lambda ()
    (let ((m (make-module)))
      m))
  #:printer write
  #:compilers
  `(;; (scheme . ,(lambda (source module wtf)
    ;;              (values
    ;;               source
    ;;               module
    ;;               module)))
    (tree-il . ,(lambda (source module wtf)
                  (values
                   (meson-ast->tree-il source)
                   module
                   module)))))
