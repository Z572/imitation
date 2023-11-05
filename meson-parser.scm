#!/usr/bin/env -S guile --no-auto-compile -e main -L .
!#
(use-modules
 (ice-9 threads)
 (ts)
 (srfi srfi-1)
 (srfi srfi-26)
 (srfi srfi-71)
 (srfi srfi-171)
 (ice-9 pretty-print)
 (ice-9 iconv)
 (ice-9 textual-ports)
 (rnrs bytevectors gnu)
 (ice-9 textual-ports)
 (language meson spec)
 (system base language)
 (system base compile)
 (system base target)
 (ice-9 match))

(define (main args)
  (define no-dump? (= (length args) 1))
  (call-with-input-file "meson.build"
    (lambda (x)
      ((if no-dump? pretty-print identity)
       (read-and-compile
        x
        #:from meson
        #:to
        (if no-dump?
            'scheme
            'value))))))
