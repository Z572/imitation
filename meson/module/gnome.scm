(define-module (meson module gnome)
  #:use-module (oop goops)
  #:use-module (meson types)
  #:export (gdbus_codegen
            generate_gir
            generate_vapi
            genmarshal
            gtkdoc
            mkenums_simple))

(define* (gdbus_codegen
          name
          #:optional
          (xml #f)
          #:key sources
          interface_prefix
          namespace
          extra_args
          autocleanup
          object_manager
          annotations
          docbook
          build_by_default
          install_dir
          install_header)
  '("gdbus_codegen1" "gdbus_codegen2"
    "gdbus_codegen3"))

(define* (gtkdoc name #:key main_xml
                 src_dir
                 dependencies
                 ignore_headers
                 scan_args
                 mkdb_args
                 gobject_typesfile
                 content_files
                 expand_content_files
                 fixxref_args
                 install)
  (pk 'gtkdoc )
  (list "gtkdoc"))
(define* (generate_gir #:rest rest)
  (pk 'generate_gir)
  (list "generate_gir"))

(define* (generate_vapi #:rest rest)
  (pk 'generate_vapi)
  (list "generate_vapi"))

(define* (mkenums_simple
          out
          source

          #:key
          function_prefix
          install_header
          install_dir
          symbol_prefix
          body_prefix
          (sources source)
          prefix
          #:rest rest)
  '("mkenums_simple" "mkenums_simple2"))


(define* (genmarshal
          out
          source

          #:key
          depends
          (sources source)
          prefix
          #:rest rest)
  '())
