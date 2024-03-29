(use-modules (language meson parse)
             (ice-9 match))

(define-syntax-rule (read* str)
  (call-with-input-string
      (string-append str
                     "\n")
    (lambda (x)
      (parse-meson
       (read-meson x (current-module))))))
(define-syntax-rule (match-s o body ...)
  (match (read* o)
    (($ <meson-definition> _ (body ...))
     #t)))

(define-syntax-rule (check name body)
  (test-assert name
    (match-s
     name
     body)))

(test-assert "read1"
  (read* "abc=1"))

(check "call()" ($ <meson-call> _ 'call () ()))
(check "call_with_arg('arg1')"
       ($ <meson-call> _ 'call_with_arg
          (($ <meson-string> _ "arg1")) ()))
(check "call_with_karg('arg1',karg1:'20')"
       ($ <meson-call> _ 'call_with_karg
          (($ <meson-string> _ "arg1"))
          (#:karg1 ($ <meson-string> _ "20"))))

(check "id" ($ <meson-id> _ 'id #f))
(check "true" ($ <meson-bool> loc #t))
(check "false" ($ <meson-bool> loc #f))
(check "[]" ($ <meson-array> loc '()))
(check "[1,2]"
       ($ <meson-array> loc
          (($ <meson-number> _ 1)
           ($ <meson-number> _ 2))))
(check "[1,]" ($ <meson-array> loc (($ <meson-number> _ 1))))
(check "{}" ($ <meson-dictionary> loc () ()))
(check "{'a':20}" ($ <meson-dictionary> _
                     (($ <meson-string> _ "a"))
                     (($ <meson-number> _ 20))))
