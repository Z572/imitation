(define-module (language meson parse)
  #:use-module (ice-9 threads)
  #:use-module (ts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors gnu)
  #:use-module (ice-9 textual-ports)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:use-module (system base target)
  #:use-module (meson types)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:export (parse-meson
            meson-ast->tree-il
            read-meson))

(define (parse-meson exp)
  (define retrans
    (lambda (x)
      (parse-meson x)))
  (define (handle-if exp)
    (match exp
      ((("if" "if")
        ("condition" condition)
        expr
        body ...
        ("endif" "endif"))

       `(if ,(retrans condition)
            ,(retrans expr)
            ,(handle-if body)))
      ((("elif" "elif")
        ("condition" condition)
        ("statement_list" . statement_list)
        body ...)
       `(if ,(retrans condition)
            (begin ,@(map retrans statement_list))
            ,(handle-if body)))
      ((("else" "else") expr)
       (retrans expr))
      (else *unspecified*)))

  (match exp
    (("build_definition" . body)
     `(build-definition ,@(map retrans body)))
    (("comment")
     `(comment))
    (("continue" _)
     'continue)
    (("break" _)
     'break)
    (("statement" . body)
     (append-map retrans body))
    (("identifier" name)
     `(%id ,(string->symbol name)))
    (("assignment_statement" ("identifier" name) ("assignment_operator" op) value)
     `(%assignment ,(string->symbol op) ,(string->symbol name) ,(retrans value)))
    (("int_literal" num) (handle-number num))
    (((or "expression" "expression_statement") o)
     (retrans o))
    (("additive_expression" arg1 ("additive_operator" op) arg2)
     (list (string->symbol op) (retrans arg1) (retrans arg2)))
    (("iteration_statement"
      ("foreach" "foreach")
      ("identifier_list" . identifier_list)
      ("expression" exp)
      body ...
      ("endforeach" "endforeach"))
     `(foreach ,(map retrans identifier_list)
               ,(retrans exp)
               (begin ,(map retrans body)) ))
    (("conditional_expression" conditional exp1 exp2)
     `(if ,(retrans conditional) ,(retrans exp1) ,(retrans exp2) ))
    (("primary_expression" exp)
     (retrans exp))
    (("subscript_expression" exp index)
     `(%subscript ,(retrans exp) ,(retrans index)))
    (("logical_and_expression" arg1  (_ op) arg2)
     `(if ,(retrans arg1) ,(retrans arg2) #f))
    (("logical_or_expression" arg1  (_ op) arg2)
     `(if ;; ,op
       ,(retrans arg1) ,(retrans arg1) ,(retrans arg2)))
    (("equality_expression" arg1 ("equality_operator" op) arg2)
     `(%equal ,(string->symbol op) ,(retrans arg1) ,(retrans arg2) ))
    (("selection_statement" body ...)
     (handle-if body))
    (("statement_list" . s)
     `(begin ,@(map retrans s)))
    (("multiplicative_expression" exp ("multiplicative_operator" op) exp2)
     (list (string->symbol op) (retrans exp) (retrans exp2)))
    (("dictionary_literal" "{}")
     `(dict))
    (("dictionary_literal" ("key_value_item" k v) ...)
     ;; `(apply list 'dict ,@(fl)
     ;;         '())
     `(dict
       ,@(zip
          (map retrans k)
          (map retrans v)))
     )
    (("array_literal" "[]") `(%vector))
    (("array_literal" . body)
     `(%vector ,@(map retrans body)))
    (("function_expression" ("identifier" name) . args)
     `(%call
       (f-id ,(string->symbol name))
       ,@(append-map (lambda (x)
                       (match (retrans x)
                         (('arg x)
                          (list x))
                         (('karg name value)
                          (list (symbol->keyword name) value))))
                     args)
       ))
    (("unary_expression" ("unary_operator" (op op)) expr)
     ;; not
     `(,(string->symbol op) ,(retrans expr)))
    (("unary_expression" ("unary_operator" "-") expr)
     ;; -
     `(- ,(retrans expr)))
    (("relational_expression" id ("relational_operator" (op op) ...) v)
     `(%relational ,(map string->symbol op) ,(retrans id) ,(retrans v)  ))
    (("relational_expression" id ("relational_operator" op) v)
     `(%relational (,(string->symbol op)) ,(retrans id) ,(retrans v)  ))
    (("argument" expr)
     `(arg ,(retrans expr)))
    (("keyword_argument" ("identifier" id) expr)
     `(karg ,(string->symbol id) ,(retrans expr)))
    (("method_expression" id fe
      ;; ("function_expression" ("identifier" name) . args)
      )
     (let ((f b (match (retrans fe)
                  ((_ name . args)
                   (values name args)))))
       `(method-call
         ,(retrans id) ,f
         ,@b)))
    (("string_literal" b)
     (substring b 1 (- (string-length b) 1)))
    (("fstring_literal" b)
     `(fstring ,(substring b 1 (- (string-length b) 1))))
    (("boolean_literal" b)
     (cond ((string= b "true") #t)
           ((string= b "false") #f)
           (else (error))))
    (("multiline_string_literal" b)
     `(multiline-string ,(substring b 3 (- (string-length b) 3))))
    (else
     `(uh ,exp))))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (pair? props) props))))
(define-public (meson-ast->tree-il exp)
  (define rerun meson-ast->tree-il)
  (define loc (location exp))
  (pk 'loc loc)
  (match exp
    (('build-definition body ...)
     (list->seq loc (append (map rerun body)
                            '()
                            ;; (list (make-call
                            ;;        loc
                            ;;        (make-module-ref loc '(meson types) '%meson #t)
                            ;;        '()))
                            )))
    (('begin body ...)
     (list->seq loc (map rerun body)))
    (#t (make-const loc #t))
    (#f (make-const loc #f))
    (('f-id id)
     (make-module-ref loc '(meson function) id #t))
    (((and f (or '* '< '>)) a b)
     (make-primcall loc f (map rerun (list a b))))

    (('%relational kw v1 lst)
     (make-call loc (rerun `(f-id %relational))
                (cons (make-const loc kw)
                      (map rerun (list v1 lst)))))
    (('%vector value ...)
     (make-call loc (make-module-ref loc '(guile) 'list #t)
                (map rerun value)))
    (('if test consequent alternate)
     (make-conditional loc (rerun test) (rerun consequent) (rerun alternate)))
    (('%equal op v1 v2)
     (make-call loc (rerun `(f-id ,op)) (map rerun (list v1 v2)) ))

    (('or v1 v2)
     (make-conditional loc (rerun v1) (rerun v1) (rerun v2)))
    (((and (or '/ '% '+ '-) v) v1 v2)
     (make-call loc (rerun `(f-id ,v)) (map rerun (list v1 v2))))

    (((and '- v) v1)
     (make-call loc (rerun `(f-id ,v)) (map rerun (list v1))))

    (('%call id ass ...)
     (make-call loc (rerun id) (map rerun ass)))
    (('method-call obj f args ...)
     (make-call loc
                (rerun `(f-id meson-method-call))
                ;; (rerun f)
                (map rerun (cons* obj f args ))))
    (('%subscript id index)
     (make-call loc (rerun `(f-id %subscript)) (map rerun (list id index))))
    (('%assignment '= (and name (? symbol?)) value)
     (make-call loc (rerun `(f-id %assignment))
                (list (make-const loc name)
                      (rerun value))))
    (('%assignment '+= (and name (? symbol?)) value)
     (make-call loc (rerun `(f-id %assignment+=))
                (list (make-const loc name)
                      (rerun value))))
    (('dict)
     (make-call
      loc
      (make-module-ref loc '(meson types) 'make-dictionarie #f)
      '()))
    (('dict a ...)
     (make-call
      loc
      (make-module-ref loc '(meson types) 'make-dictionarie #f)
      (map (match-lambda ((s o) (make-call loc
                                           (make-module-ref loc '(guile) 'list #t)
                                           (list (rerun s) (rerun o))))) a)))
    (('not a)
     (make-call
      loc (make-module-ref loc '(guile) 'not #t)
      (list (rerun a))))
    (('multiline-string str)
     (rerun str))
    (('%id name)
     ;; (make-toplevel-ref loc #f name)
     (make-call loc (rerun `(f-id %get-id))
                (list (make-const loc name)))
     )
    ((? unspecified?)
     (make-void loc))
    ((and (or (? string?) (? number?) (? keyword?)) obj)
     (make-const loc obj))
    (('comment)
     (make-void loc))
    (a a)))

(define (handle-number n)
  (let ((n (string-downcase n)))
    (string->number
     (cond ((< (string-length n) 2)
            n)
           ((member (substring n 0 2)
                    '("0x" "0b" "0o"))
            (string-replace n "#" 0 1 ))
           (else n)))))

(define (find-child node proc)
  (let* ((er-node (find proc (ts-node-childs node ))))
    (or (and=> er-node (cut find-child <> proc))
        er-node)))

(define (find-up-pos-is-0 node)
  (let loop ((node node))
    (if (zero? (cdr (ts-node-start-point node)))
        node
        (loop (ts-node-parent node)))))

(define parser
  (delay (ts-parser-new
          #:language
          (get-ts-language-from-file
           ;; /gnu/store/z7rn6i3bd96il9lcr7l4ykh6imr8a267-tree-sitter-meson-1.2-0.3d6dfbd/lib/tree-sitter/
           "libtree-sitter-meson.so"
           "tree_sitter_meson"))))

(define* (skfjdsfjs str #:key (bg 0))
  (cons (if (zero? bg)
            "-8<---------------------------------------------->8-"
            (string-append "     |" (%meson-current-directory) "/meson.build"
                           "
-8<--+------------------------------------------->8-"))
        (vector->list
         (vector-map (lambda (n x) (string-append (number->string (+ bg n)) " |" x))
                     (list->vector (string-split str #\newline))))))

(define (read-meson port env)
  (let ((s (get-string-all port)))
    (define (node-string n)
      (substring-utf8
       s
       (ts-node-start-byte n)
       (ts-node-end-byte n)))

    (if (string-null? s)
        the-eof-object
        (let loop ((rn (ts-tree-root-node (ts-parser-parse-string (force parser) s))))
          (when (ts-node-has-error? rn)
            (and-let* ((n (find-child rn ts-node-has-error?))
                       (parent (find-up-pos-is-0 n) ))
              (format #t "I get a error!
~a
~a
~S ~a--~a
"
                      (string-join
                       (skfjdsfjs (node-string parent)
                                  #:bg (car (ts-node-start-point parent)))
                       "\n")

                      (string-join (map (lambda (str)

                                          (string-append (make-string (+ 4 (cdr (ts-node-start-point n))) #\space)
                                                         str
                                                         "\n"))
                                        '("^" "I think is this is a error")))

                      (node-string n)
                      (ts-node-start-point n) (ts-node-end-point n))
              ;; (exit 1 )
              )
            )
          (let ((childs (ts-node-childs rn #t)))
            (if (null? childs)
                (if (ts-node-extra? rn)
                    #f
                    (list (ts-node-type rn)
                          (node-string rn)))
                (if (string=?
                     (ts-node-type rn) "string_literal")

                    (list (ts-node-type rn)
                          (node-string rn))

                    (cons (ts-node-type rn)
                          (filter-map loop childs)))))))))
