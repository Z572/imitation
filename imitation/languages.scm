(define-module (imitation languages)
  #:use-module (ts language))

(define-public ts-meson
  (delay
    (get-ts-language-from-file
     "libtree-sitter-meson.so"
     "tree_sitter_meson")))
