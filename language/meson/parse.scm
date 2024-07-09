(define-module (language meson parse)
  #:use-module (imitation languages)
  #:use-module (ice-9 format)
  #:use-module (imitation utils)
  #:use-module (oop goops)
  #:use-module (texinfo string-utils)
  #:use-module (srfi srfi-71)
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

(define-once %default-properties
  (make-parameter '()))
(define-class <meson-ast> ()
  (properties
   #:getter meson-ast-properties
   #:init-thunk %default-properties #:init-keyword #:properties))
(define-method (print-location (o <meson-ast>) port)
  (match (meson-ast-properties o)
    ((('filename . f) ('line . line) ('column . column))
     (format port "~a:~a:~a" f line column))
    (else (format port "#f:#f:#f"))))

(define-method (write (d <meson-ast>) port)
  (format port "#<~a '~a' ~x>"
          (class-name (class-of d))
          (print-location d #f)
          (object-address d) ))

(define-class <meson-container> ()
  (value  #:getter .value #:init-keyword #:value))

(let-syntax ((defclass
               (syntax-rules ()
                 ((_ n (cl ...)acc ...)
                  (begin (define-class n (cl ... <meson-ast>)
                           acc ...)
                         (export n))))))
  (defclass <meson-string> (<meson-container>)
    ;; (value  #:init-keyword #:value)
    (multiline? #:init-value #f #:init-keyword #:multiline?)
    (fstring? #:init-value #f #:init-keyword #:fstring?))

  (define-method (write (d <meson-container>) port)
    (format port "#<~a '~S' '~a' ~x>"
            (class-name (class-of d))
            (.value d)
            (print-location d #f)
            (object-address d) ))

  (defclass <meson-comment> ()
    (comment #:init-keyword #:comment))
  (defclass <meson-definition> (<meson-container>))
  (defclass <meson-bool> (<meson-container>))
  (defclass <meson-array> (<meson-container>))
  (defclass <meson-number> (<meson-container>)
    ;; (value  #:init-keyword #:value)
    )
  ;; (define-method (write (d <meson-number>) port)
  ;;   (format port "#<~a '~S' '~a' ~x>"
  ;;           (class-name (class-of d))
  ;;           (.value d)
  ;;           (print-location d #f)
  ;;           (object-address d) ))
  (defclass <meson-dictionary> ()
    (keywords
     #:init-value '()
     #:getter meson-dictionary-keywords #:init-keyword #:keywords)
    (values
     #:init-value '()
     #:getter meson-dictionary-values #:init-keyword #:values))
  (define-method (write (d <meson-dictionary>) port)
    (format port "#<~a keywords: ~a values: ~a '~a' ~x>"
            (class-name (class-of d))
            (meson-dictionary-keywords d)
            (meson-dictionary-values d)
            (print-location d #f)
            (object-address d)))

  (defclass <meson-operator> ()
    (name #:getter meson-operator-name #:init-keyword #:name)
    (items #:getter meson-operator-items #:init-keyword #:items #:init-value (list)))

  (defclass <meson-assignment> (<meson-operator>))
  (define-method (write (d <meson-operator>) port)
    (format port "#<~a '~a' '~a' '~a' ~x>"
            (class-name (class-of d))
            (meson-operator-name d)
            (meson-operator-items d)
            (print-location d #f)
            (object-address d) ))
  (defclass <meson-call> ()
    (name #:getter meson-call-name #:init-keyword #:name)
    (args #:getter meson-call-args #:init-keyword #:args)
    (kwargs #:getter meson-call-kwargs #:init-keyword #:kwargs #:init-value (list)))

  (define-method (write (d <meson-call>) port)
    (format port "#<~a '~S' args: ~a kwargs: ~a '~a' ~x>"
            (class-name (class-of d))
            (meson-call-name d)
            (meson-call-args d)
            (meson-call-kwargs d)
            (print-location d #f)
            (object-address d)))

  (defclass <meson-foreach> ()
    (identifiers #:getter meson-foreach-identifiers #:init-keyword #:identifiers)
    (expr #:getter meson-foreach-expr #:init-keyword #:expr)
    (body #:getter meson-foreach-body #:init-keyword #:body))
  (defclass <meson-id> ()
    (name #:getter meson-id-name #:init-keyword #:name)
    (temp? #:accessor meson-id-temp?
           #:init-keyword
           #:temp?
           #:init-value #f))
  (defclass <meson-if> ()
    (condition #:accessor meson-if-condition
               #:init-keyword
               #:condition)
    (then #:accessor meson-if-then
          #:init-keyword
          #:then)
    (else #:accessor meson-if-else
          #:init-keyword
          #:else))
  (define-class <meson-conditional> (<meson-if>))
  (define-method (write (d <meson-id>) port)
    (format port "#<~a '~S' ~a '~a' ~x>"
            (class-name (class-of d))
            (meson-id-name d)
            (meson-id-temp? d)
            (print-location d #f)
            (object-address d)))
  (define (get-id id)
    (meson-id-name id)))

(define meson-ast-location meson-ast-properties)

(define (parse-kw o)
  (match o
    (("not" "not")
     'not)
    (("in" "in")
     'in)
    (((or "multiplicative_operator" "equality_operator") op)
     (string->symbol op))))

(define* (parse-meson exp* #:optional (tmpvars '()))
  (define exp (syntax->datum exp*))
  (define -loc (lambda (x) (pk '-loc x)(syntax-source x)))
  (define retrans
    (lambda* (x #:optional (tmpvars tmpvars))
      (parse-meson x tmpvars)))
  (define (get-id name)
    (match (retrans name)
      (($ <meson-id> loc id temp?) id)
      (else (error 'get-id "~a" name))))
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

  (define e (match exp
              (("build_definition" . body)
               (make <meson-definition>
                 #:value (map retrans body)
                 #:properties -loc))
              (("comment")
               (make <meson-comment>
                 #:properties -loc))
              (("continue" _)
               'continue)
              (("break" _)
               'break)
              (("statement" . body)
               (append-map retrans body))
              (("identifier" name)
               (make <meson-id>
                 #:name (string->symbol name)
                 #:properties -loc
                 #:temp? (member (string->symbol name)
                                 (map meson-id-name tmpvars))))
              (("assignment_operator" name )
               (make <meson-operator> #:properties -loc #:name (string->symbol name)))
              (("additive_operator" name)
               (make <meson-operator> #:properties -loc #:name (string->symbol name)))
              (("assignment_statement" id op value)
               `(%assignment ,(retrans op) ,(get-id id) ,(retrans value)))
              (("int_literal" num)
               (make <meson-number>
                 #:properties -loc
                 #:value (handle-number num)))
              (((or "expression" "expression_statement" "primary_expression") o)
               (retrans o))
              (("additive_expression" arg1 op arg2)
               (list (retrans op) (retrans arg1) (retrans arg2)))
              ;; (("foreach" "foreach")
              ;;  'foreach)
              (("endforeach" "endforeach")
               'endforeach)
              (("identifier_list" identifier_list)
               (list (retrans identifier_list)))
              (("identifier_list" identifier_list ...)
               (map retrans identifier_list))
              (("iteration_statement"
                _
                identifiers
                exp
                body ...
                (? (lambda (x) (equal? (retrans x) 'endforeach))))
               (let ((ids (retrans identifiers)))

                 (make <meson-foreach>
                   #:identifiers ids
                   #:expr (retrans exp)
                   #:body (map (lambda (x)
                                 (retrans x ids))
                               body)
                   #:properties -loc)))
              (("conditional_expression" conditional exp1 exp2)
               (make <meson-conditional>
                 #:properties -loc
                 #:condition (retrans conditional)
                 #:then (retrans exp1)
                 #:else (retrans exp2)))
              (("subscript_expression" exp index)
               `(%subscript ,(retrans exp) ,(retrans index)))
              (("logical_and_expression" arg1 _ arg2)
               `(if ,(retrans arg1) ,(retrans arg2) #f))
              (("logical_or_expression" arg1 _ arg2)
               `(if ,(retrans arg1) ,(retrans arg1) ,(retrans arg2)))
              (("equality_expression" arg1 equality_operator arg2)
               `(%equal ,(retrans equality_operator) ,(retrans arg1) ,(retrans arg2)))
              (("selection_statement" body ...)
               (pk 'if (handle-if body)))
              (("statement_list" . s)
               `(begin ,@(map retrans s)))
              (((or "multiplicative_operator" "equality_operator") op)
               (string->symbol op))
              (("multiplicative_expression" exp multiplicative_operator exp2)
               (list (retrans multiplicative_operator) (retrans exp) (retrans exp2)))
              (("key_value_item" k v)
               (list (retrans k) (retrans v)))
              (("dictionary_literal" "{}")
               ;; `(dict)
               (make <meson-dictionary> #:properties -loc)
               )
              (("dictionary_literal" i ...)
               (let ((k+v (map retrans i)))
                 (make <meson-dictionary> #:properties -loc
                       #:keywords (map car k+v)
                       #:values (map second k+v))))
              (("array_literal" "[]")
               (make <meson-array> #:properties -loc #:value '()))
              (("array_literal" . body)
               (make <meson-array> #:properties -loc #:value (map retrans body)))
              (("function_expression" name . args)
               (make <meson-call>
                 #:properties -loc
                 #:name (get-id name)
                 #:args (concatenate
                         (filter-map (lambda (x)
                                       (match (retrans x)
                                         (('arg x)
                                          (list x))
                                         (('karg name value)
                                          #f)
                                         (else #f)))
                                     args))
                 #:kwargs (concatenate
                           (filter-map (lambda (x)
                                         (match (retrans x)
                                           (('karg name value)
                                            (list (symbol->keyword name) value))
                                           (('arg x)
                                            #f)
                                           (else #f)))
                                       args))
                 ))
              (("unary_operator" op)
               (retrans op))
              (("unary_expression" op expr)
               ;; not
               `(,(retrans op) ,(retrans expr)))
              (("unary_expression" ("unary_operator" "-") expr)
               ;; -
               `(- ,(retrans expr)))
              (("not" "not")
               'not)
              (("in" "in")
               'in)
              (("relational_operator" op) op)
              (("relational_expression" id relational_operator v)
               `(%relational ,(retrans relational_operator) ,(retrans id) ,(retrans v)  ))
              (("argument" expr)
               `(arg ,(retrans expr)))
              (("keyword_argument" id expr)
               `(karg ,(get-id id) ,(retrans expr)))
              (("method_expression" id fe)
               (let ((f b (match (retrans fe)
                            ((_ name args kwargs)
                             (values name (append args kwargs)))
                            (($ <meson-call> loc f args kwargs)
                             (values f (append args kwargs)))
                            (o (error 'f-b o)))))
                 `(method-call
                   ,(retrans id) ,f
                   ,@b)))
              (("string_literal" b)
               (if (string? b)
                   (make <meson-string>
                     #:properties -loc
                     #:value (substring b 1 (- (string-length b) 1)))
                   (error 'k)))
              (("fstring_literal" b)
               (if (string? b)
                   (make <meson-string>
                     #:fstring? #t
                     #:properties -loc
                     #:value (substring b 1 (- (string-length b) 1)))
                   (error 'for-now-no-support-fstring! ""))
               ;; `(fstring ,(substring b 1 (- (string-length b) 1)))
               )
              (("boolean_literal" b)
               (cond ((string= b "true")
                      (make <meson-bool> #:properties -loc #:value #t))
                     ((string= b "false")
                      (make <meson-bool> #:properties -loc #:value #f))
                     (else (error))))
              (("multiline_string_literal" b)
               (make <meson-string>
                 #:multiline? #t
                 #:properties -loc
                 #:value (substring b 3 (- (string-length b) 3)))
               ;; `(multiline-string ,(substring b 3 (- (string-length b) 3)))
               )
              (else
               `(uh ,exp))))
                                        ;(cons e (cdr exp*))
  e)

(define (location x)
  (if (is-a? x <meson-ast>)
      (meson-ast-location x)
      (and (pair? x)
           (let ((props (source-properties x)))
             (and (pair? props) props))))
  )

(define (mk-ts-list loc)
  (make-module-ref loc '(guile) 'list #t))

(define-public (meson-ast->tree-il exp)
  (define rerun meson-ast->tree-il)
  (define loc (location exp))
  (define* (f-id id #:optional (loc loc))
    (make-module-ref loc '(meson function) id #t))

  (match exp
    (($ <meson-definition> loc body)
     (list->seq
      loc (append (map rerun body)
                  '())))
    ;; (#t (make-const loc #t))
    ;; (#f (make-const loc #f))
    ;; (('f-id id)
    ;;  (make-module-ref loc '(meson function) id #t))
    (((and f (or '* '< '>)) a b)
     (make-primcall loc f (map rerun (list a b))))

    (($ <meson-array> loc value)
     (make-call loc (mk-ts-list loc)
                (map rerun value)))
    (('if test consequent alternate)
     (make-conditional loc (rerun test) (rerun consequent) (rerun alternate)))
    (('%equal op v1 v2)
     (make-call loc (f-id op) (map rerun (list v1 v2))))

    (('or v1 v2)
     (make-conditional loc (rerun v1) (rerun v1) (rerun v2)))
    (((and (or '/ '% '+ '-) v) v1 v2)
     (make-call loc (f-id v) (map rerun (list v1 v2))))

    (($ <meson-assignment> loc op (name value))

     (make-call loc (f-id (match op
                            ('= '%assignment)
                            ('+= '%assignment+=)
                            ('-= '%assignment-=))
                          loc)
                (list (make-const loc name)
                      (rerun value))))
    (($ <meson-operator> loc op vs)
     (make-call loc (f-id op loc) (map rerun vs)))
    (($ <meson-call> loc f n kws)
     (make-call
      loc
      (f-id 'meson-call loc)
      (list (make-const loc f)
            (make-call loc (mk-ts-list loc)
                       (map rerun n))
            (make-call loc (mk-ts-list loc)
                       (map rerun kws)))))
    (('method-call obj f args ...)
     (make-call loc
                (f-id 'meson-method-call loc)
                (cons*
                 (rerun obj)
                 (make-const
                  loc
                  (match f
                    (($ <meson-id> _ id)
                     id)
                    ((? symbol? o) o)
                    (_ (error '-merror ""))))
                 (map rerun args))))
    (('%subscript id index)
     (make-call loc (f-id '%subscript loc) (map rerun (list id index))))

    ;; (('%assignment '+= (and name (? symbol?)) value)
    ;;  (make-call loc (f-id '%assignment+=)
    ;;             (list (make-const loc name)
    ;;                   (rerun value))))
    ;; (('dict)
    ;;  (make-call
    ;;   loc
    ;;   (make-module-ref loc '(meson types) 'make-dictionarie #f)
    ;;   '()))
    (($ <meson-dictionary> loc keys values)
     (make-call
      loc
      (make-module-ref loc '(meson types) 'make-dictionarie #f)
      (list (make-call loc (mk-ts-list loc)
                       (map rerun keys))
            (make-call loc (mk-ts-list loc)
                       (map rerun values)))))
    (('not a)
     (make-call
      loc (make-module-ref loc '(guile) 'not #t)
      (list (rerun a))))
    (('multiline-string str)
     (rerun str))
    (($ <meson-id> loc name temp?)
                                        ;(pk 'meson-id loc name temp?)
     (if temp?
         (make-lexical-ref loc name name)
         (make-call
          loc
          (f-id '%get-id)
          (list (make-const loc name)))))
    ((? unspecified?)
     (make-void loc))
    ((and (? keyword?) obj)
     (make-const loc obj))
    (($ <meson-string> loc value multiline? fstring?)
     (make-const loc value))
    (($ <meson-foreach> loc ids expr body)
     (let* ((identifiers (map rerun ids)))
       (make-call
        loc (f-id 'meson-foreach)
        (list
         ;; (make-call
         ;;  loc
         ;;  (mk-ts-list loc)
         ;;  identifiers)
                                        ;(make-const loc (map get-id ids))
         (rerun expr)
         (make-lambda
          loc
          '()
          (make-lambda-case
           loc
           (map get-id ids)
           #f #f #f '()
           (map
            (lambda (x)
              (get-id x)
              ;; (gensym (symbol->string (get-id x)))
              )
            ids)

           (list->seq loc (map rerun body))
           #f)))))
     )
    (($ <meson-number> loc value)
     (make-const loc value))
    (($ <meson-bool> loc value)
     (make-const loc value))
    (($ <meson-comment>)
     (make-void loc))
    ((? symbol? o)
     (make-const loc o))
    ;; (a (make-const loc a))
    ))

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
  (delay
    (ts-parser-new
     #:language
     (get-ts-language-from-file
      "libtree-sitter-meson.so"
      "tree_sitter_meson"))))

(define* (skfjdsfjs str #:key (bg 0)
                    (err #f)
                    (before 0))
  (cons (if (zero? bg)
            "-8<---------------------------------------------->8-"
            (string-append "    |" (%meson-current-directory) "/meson.build"
                           "\n"
                           (center-string "ctx" 80 #\=)
                           ))
        (let ((be af (split-at (vector->list
                                (vector-map
                                 (lambda (n x)
                                   (string-append
                                    (left-justify-string
                                     (number->string (+ bg n))
                                     4)
                                    "|" x))
                                 (list->vector (string-split str #\newline))))
                               (1+ (- before bg)))))
          (append be err af))))

(define (handle-number n)
  (let ((n (string-downcase n)))
    (string->number
     (cond ((< (string-length n) 2)
            n)
           ((member (substring n 0 2)
                    '("0x" "0b" "0o"))
            (string-replace n "#" 0 1 ))
           (else n)))))

(define* (read-meson port #:optional (env #f))
  (let ((s (if (port? port) (get-string-all port) port)))
    (define filename (if (port? port) (port-filename port) #f))
    (define (node-string n)
      (substring-utf8
       s
       (ts-node-start-byte n)
       (ts-node-end-byte n)))

    (if (string-null? s)
        the-eof-object
        (let loop ((rn (ts-tree-root-node (ts-parser-parse-string
                                           (ts-parser-new
                                            #:language (force ts-meson))
                                           s))))

          (define source
            `((filename . ,filename)
              (line . ,(car (ts-node-start-point rn)))
              (column . ,(cdr (ts-node-start-point rn)))))
          (parameterize ((%default-properties source))
            (let ((childs (ts-node-childs rn #t)))
              (match (ts-node-type rn)
                ("comment"
                 (make <meson-comment>
                   #:comment
                   (substring (node-string rn) 1)))
                ("build_definition"
                 (make <meson-definition>
                   #:value (map loop childs)))
                ("function_expression"
                 (make <meson-call>
                   #:name (string->symbol
                           (node-string (ts-node-child-by-field-name
                                         rn "function")))
                   #:args (filter-map (lambda (x)
                                        (match (ts-node-type x)
                                          ("argument"
                                           (loop (ts-node-child x 0 #t)))
                                          (else #f)))
                                      childs)
                   #:kwargs
                   (concatenate
                    (filter-map (lambda (x)
                                  (match (ts-node-type x)
                                    ("keyword_argument"
                                     (list (symbol->keyword
                                            (string->symbol
                                             (node-string
                                              (ts-node-child-by-field-name
                                               x "keyword"))))
                                           (loop (ts-node-child x 1 #t))))
                                    (else #f)))
                                childs))))
                ("statement"
                 (append-map loop childs))
                ("method_expression"
                 (match childs
                   ((id fe)
                    (let ((f b (match (loop fe)
                                 ((_ name args kwargs)
                                  (values name (append args kwargs)))
                                 (($ <meson-call> loc f args kwargs)
                                  (values f (append args kwargs)))
                                 (o (error 'f-b o)))))
                      `(method-call
                        ,(loop id) ,f
                        ,@b)))))
                ("subscript_expression"
                 (match childs
                   ((exp index)
                    `(%subscript ,(loop exp) ,(loop index)))))
                ("int_literal"
                 (make <meson-number>
                   #:value (handle-number (node-string rn))))
                ("array_literal"
                 (make <meson-array>
                   #:value (map loop childs)))
                ("identifier"
                 (make <meson-id>
                   #:name (string->symbol (node-string rn))
                   ;; #:temp? (member (string->symbol name)
                   ;;                 (map meson-id-name tmpvars))
                   ))
                ("dictionary_literal"
                 (let ((key value (unzip2
                                   (map
                                    (lambda (node)
                                      (list (loop (ts-node-child-by-field-name node "key"))
                                            (loop (ts-node-child-by-field-name node "value"))))
                                    childs))))

                   (make <meson-dictionary> #:keywords key #:values value)))
                ("string_literal"
                 (let ((str (node-string rn)))
                   (make <meson-string>
                     #:value (substring str 1 (- (string-length str) 1)))))
                ("multiline_string_literal"
                 (let ((str (node-string rn)))
                   (make <meson-string>
                     #:value (substring str 3 (- (string-length str) 3)))))
                ("unary_expression"
                 (match childs
                   ((op exp)
                    (make <meson-operator>
                      #:name (string->symbol (node-string op))
                      #:items (list (loop exp))))))
                ((or "assignment_statement" "additive_expression" "multiplicative_expression")
                 (define assignment? (string= (ts-node-type rn) "assignment_statement"))
                 (match childs
                   ((exp op exp2)
                    (make (if assignment?
                              <meson-assignment>
                              <meson-operator>)
                      #:name (string->symbol (node-string op))
                      #:items (list
                               ((if assignment? (compose string->symbol node-string)
                                    loop)
                                exp)
                               (loop exp2))))))
                ("relational_expression"
                 (match childs
                   ((obj op items)
                    (make <meson-call>
                      #:name '%relational
                      #:args (list
                              (string->symbol (node-string op))
                              (loop obj) (loop items))))))
                ("boolean_literal"
                 (make <meson-bool>
                   #:value
                   (string= (node-string rn) "true")))
                ((or "expression" "expression_statement" "primary_expression")
                 (append-map loop childs))
                ("ERROR"

                 (error 'error (node-string rn)))
                (else (if (null? childs)
                          (if (ts-node-extra? rn)
                              #f
                              (datum->syntax
                               #f (list (ts-node-type rn)
                                        (node-string rn))
                               #:source
                               `((filename . ,(port-filename port))
                                 (line . ,(car (ts-node-start-point rn)))
                                 (column . ,(cdr (ts-node-start-point rn))))))
                          (cons else (map loop childs))))
                )
              ))))))
