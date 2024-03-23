#!/usr/bin/env -S guile --no-auto-compile -L . -e main
!#

(define-public (main argc . argv)
  (pk 'arg argc argv))
