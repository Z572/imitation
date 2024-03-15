#!/usr/bin/env -S guix shell guile guile-ts -- guile --no-auto-compile -L .
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

(pk 'v (compile-and-load
        (second (program-arguments))
        #:from meson
        #:to 'value))
(describe (%meson))
(module-for-each pk (.variables (%meson)))
