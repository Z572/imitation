#!/usr/bin/env -S guile --no-auto-compile -L .
!#
(use-modules
 (ice-9 threads)
 (ts)
 (srfi srfi-1)
 (srfi srfi-26)
 (srfi srfi-71)
 (srfi srfi-171)
 (ice-9 pretty-print)
 (oop goops describe)
 (meson types)
 (ice-9 iconv)
 (ice-9 textual-ports)
 (rnrs bytevectors gnu)
 (ice-9 textual-ports)
 (language meson spec)
 (system base language)
 (system base compile)
 (system base target)
 (ice-9 match))

(parameterize ((%meson-current-directory (dirname (second (program-arguments)))))
  (compile-and-load
   (second (program-arguments))
   #:from meson
   #:to 'value))

(define* (describe-meson #:optional (meson (%meson)))
  (format #t "meson project~%")
  (module-for-each (lambda (x n)
                     (format #t "var ~a: value: ~S~%" x (variable-ref n)))
                   (.variables meson))
  (hash-for-each (lambda (x n)
                   (format #t "option ~a: value: ~S~%" x n)) (.options meson)))

(describe-meson)
