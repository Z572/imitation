(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages pkg-config)
 (gnu packages tree-sitter)
 (guix build-system gnu)
 (guix build-system guile)
 (guix transformations)
 (guix gexp)
 (guix git-download)
 (guix packages))

(define-public tree-sitter-meson
  ;; tag 1.2 is Aug 24,2022  this commit is Feb 28,2023
  (let ((commit "45a11dc585c73aa104c21a6ddb12d0b822f7dda8")
        (revision "0"))
    ((@@ (gnu packages tree-sitter)tree-sitter-grammar)
     "meson" "Meson"
     "16cf30sml0g55ffqh0zi7wykhmslpjcm7p6hn2snyipnfijkx1sd"
     (git-version "0.1" revision commit)
     #:repository-url "https://github.com/Z572/tree-sitter-meson"
     #:commit commit
     #:license license:expat)))

(define imitation
  (package
    (name "imitation")
    (version "0.1")
    (source (local-file "." (string-append name "-checkout")
                        #:recursive? #t
                        #:select? (git-predicate (dirname (current-filename)))))
    (build-system guile-build-system)
    (arguments
     (list #:make-flags #~'("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list guile-3.0-latest))
    (inputs (list guile-3.0-latest ))
    (propagated-inputs (list
                        ((options->transformation
                          '((with-commit . "guile-ts=958a045fa1b2c1eb73bf46a833894a8684c7a861")))
                         guile-ts)
                        tree-sitter-meson))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
imitation
