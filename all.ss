
;; save the nicer chez behavior
(define chez-printf printf)
(define chez-pretty-print pretty-print)


;; use the nicer chez behavior for these
(define sllgen:pretty-print chez-pretty-print)
(define eopl:pretty-print chez-pretty-print)
(define define-datatype:pretty-print chez-pretty-print)


;;I do not want to get into the debugger:
(define eopl:error-stop (lambda () '()))

(define eopl:error errorf)

;-------------------------- from this other file:

;; define-datatype.scm 

;; this line must be within 8 lines of the top of the file
'(let ((time-stamp "Time-stamp: <2001-06-08 10:36:53 dfried>"))
  (display (string-append
             "define-datatype.scm version J3 "
             (substring time-stamp 13 29)
             (string #\newline))))

;;; This is an r5rs-compliant datatype system.

;;; exports define-datatype, isa, cases, list-of?, always?
;;; test with (define-datatype:test-all)



;; new error reporting system added by mw Mon Apr 24 14:49:03 2000.
(define define-datatype:report-error eopl:error)
;   (lambda (symbol format . data)
;     ;; print the message
;     (eopl:printf "Error in ~s: " symbol)
;     (apply eopl:printf (cons format data))
;     (newline)
;     (eopl:error-stop)))  


(define define-datatype:reset-registries 'ignored)
(define define-datatype:is-a-type? 'ignored)
(define define-datatype:datatype-checker&registry-updater 'ignored)
(define define-datatype:case-checker 'ignored)

(let ((define-datatype:type-registry '())
      (define-datatype:variant-registry '()))  

  (set! define-datatype:reset-registries
    (lambda ()
      (set! define-datatype:type-registry '())
      (set! define-datatype:variant-registry '())
      #t))

  (set! define-datatype:is-a-type?
    (lambda (type-name)
      (memq type-name define-datatype:type-registry)))

  (set! define-datatype:datatype-checker&registry-updater
    (letrec ((set?
               (lambda (s)
                 (if (null? s) #t
                   (and (not (memq (car s) (cdr s))) (set? (cdr s)))))))
      (lambda (Type-name Variants)
        (if (not (symbol? Type-name))
          (define-datatype:report-error 'define-datatype
            " The data type name ~s is not an identifier."
            Type-name))
        (for-each
          (lambda (variant)
            (if (not (symbol? (car variant)))
              (define-datatype:report-error 'define-datatype
                (string-append
                  "(While defining the ~a datatype) "
                  "  The variant-name ~s is not an identifier.")
                Type-name (car variant))))
          Variants)
        (let ((variant-names (map car Variants)))
          (if (not (set? variant-names))
            (define-datatype:report-error 'define-datatype
              (string-append
                "(While defining the ~a datatype) "
                "  Some of the variant-names are repeated: ~s.")
              Type-name variant-names))
          (for-each
            (lambda (v)
              (cond  ;;; This assq cannot be changed.
                ((assq v define-datatype:variant-registry) =>
                 (lambda (pair)
                   (if (not (eq? (cdr pair) Type-name))
                     (define-datatype:report-error 'define-datatype
                       (string-append
                         "(While defining the ~a data type) "
                         "  The variant-name ~s has already been "
                         "  used as a variant name in ~s.")
                       Type-name v (cdr pair)))))))
            variant-names)
          (cond ;;; This assq could be a memq over variant names, only.
                ;;; but would reqire a third local registry.
            ((assq Type-name define-datatype:variant-registry) =>
             (lambda (pair)
               (define-datatype:report-error 'define-datatype
                 (string-append
                   "(While defining the ~a data type) "
                   "  The type name ~s has already been "
                   "  used as a variant name ~s in the "
                   "  data type ~s.")
                 Type-name Type-name (car pair) (cdr pair))))
            ((memq Type-name variant-names)
             (define-datatype:report-error 'define-datatype
               (string-append
                 "(While defining the ~a data type) "
                 "  Variant name is the same as the data type name.")
               Type-name)))
          (for-each
            (lambda (variant-name)
              (cond
                ((memq variant-name define-datatype:type-registry)
                 (define-datatype:report-error 'define-datatype
                   (string-append
                     "(While defining the ~a data type) "
                     "  The variant name ~s has already been "
                     "  used as a type name.")
                   Type-name variant-name))))
            variant-names)
          (set! define-datatype:variant-registry
            (append
              (map (lambda (v) (cons v Type-name)) variant-names)
              define-datatype:variant-registry))
          (cond 
            ((memq Type-name define-datatype:type-registry) =>
             (lambda (pair)
               (set-car! pair Type-name)))
            (else
              (set! define-datatype:type-registry
                (cons Type-name define-datatype:type-registry))))))))
  
  (set! define-datatype:case-checker
    (let ((remq-or-false
            (lambda (sym ls)
              (call-with-current-continuation
                (lambda (k)
                  (let f ((ls ls))
                    (cond ((null? ls) (k #f))
                      ((eq? (car ls) sym) (cdr ls))
                      (else (cons (car ls) (f (cdr ls)))))))))))
      (lambda (Type-value Type-name Expression clauses)
        (if (eq? Type-name Expression)
          (begin
            (define-datatype:report-error 'cases
              (string-append
                "The data type ~s should not be the same "
                "  as a lexical variable.")
              Type-name))
          (let ((variant-table (cdr Type-value)))
            (let f ((clauses* clauses)
                    (unused-variants (map car variant-table)))
              (if (null? clauses*)
                (if (not (null? unused-variants))
                  (begin
                    (define-datatype:report-error 'cases "Missing variant clauses for ~s."
                      unused-variants)))
                (let* ((head-clause (car clauses*))
                       (tail-clauses (cdr clauses*))
                       (purported-variant (car head-clause)))
                  (if (eq? purported-variant Expression)
                    (begin
                      (define-datatype:report-error 'cases
                        (string-append
                          "The variant name ~s should not be the same "
                          "  as a lexical variable.")
                        Expression))
                    (cond
                      ((and (null? tail-clauses) (eq? purported-variant 'else))
                 ; do nothing, we're fine
                       )                        
                      ((assq purported-variant variant-table)
                       =>
                       (lambda (p)
                         (let ((fields (cdr p))
                               (purported-fields (cadr head-clause))
                               (new-unused-variants-or-false
                                 (remq-or-false
                                   purported-variant
                                   unused-variants)))
                           (if (not (=
                                      (length fields)
                                      (length purported-fields)))
                             (begin
                               (define-datatype:report-error 'cases "Bad fields in ~s." head-clause)))
                           (if (not new-unused-variants-or-false)
                             (begin
                               (define-datatype:report-error 'cases "Duplicate variant clause: ~s."
                                 head-clause)))
                           (f tail-clauses new-unused-variants-or-false))))
                      (else
                       (define-datatype:report-error 'cases
                          "Bad clause: ~s."
                          head-clause)))))))))))))

(define-syntax isa
  (syntax-rules ()
    ((_)
     (define-datatype:report-error 'isa "isa expects 1 argument, not 0."))
    ((_ type-name)
     (if (symbol? 'type-name)
       (lambda args
         (if (null? args)
           (define-datatype:report-error 'isa "(isa ~s) expects 1 argument, not 0." 'type-name)
           (if (null? (cdr args))
             (let ((variant (car args)))
               (let ((type-info type-name)) 
                 (if (and (pair? type-info) (list? (car type-info)))
                   (and (pair? variant)
                     (memq (car variant) (car type-info)) #t)
                   (define-datatype:report-error 'isa
                     (string-append
                       "(isa ~s) did not get a data type bound to an "
                       "  appropriate structure: ~s. "
                       "  This tends to happen when the type name is "
                       "  bound to a lexical variable.")
                     'type-name type-info))))
             (define-datatype:report-error 'isa
               (string-append
                 "(isa ~s) expects 1 argument, not ~s. "
                 "  With argument list = ~s.")
               'type-name (length args) args))))
       (define-datatype:report-error 'isa "Type name is not a symbol: ~s." 'type-name)))
    ((_  type-name other ...)
     (define-datatype:report-error 'isa "(isa ~s) expects 1 argument, not ~s with ~s."
       'type-name (add1 (length '(other ...)))
       (cons 'isa '(type-name other ...))))))

(define-syntax define-datatype
  (syntax-rules ()
    ((_ Type-name)
     (define-datatype:report-error 'define-datatype
       (string-append
         "   There are no variants:    ~s.")
       '(define-datatype Type-name)))
    ((_ Type-name Type-name?)
     (define-datatype:report-error 'define-datatype
       (string-append
         "   There are no variants:    ~s.")
       '(define-datatype Type-name Type-name?)))
    ((_ Type-name Type-name?
       (Variant-name (Field-name Pred?) ...)
       ...)
     (begin
       ;[wdc]
       (define ignored
               (define-datatype:datatype-checker&registry-updater
               'Type-name 
               '((Variant-name (Field-name Pred?) ...)
                 ...)))
       ;[\wdc]
       (define Type-name
         (cons '(Variant-name ...)
           '((Variant-name Field-name ...) ...)))
       (define Type-name?
         (if (symbol? 'Type-name)
           (lambda args
             (if (null? args)
               (define-datatype:report-error 'Type-name? "expects 1 argument, not 0.")
               (if (null? (cdr args))
                 (let ((variant (car args)))
                   (let ((type-info Type-name)) 
                     (if (and (pair? type-info) (list? (car type-info)))
                       (and (pair? variant)
                         (memq (car variant) (car type-info)) #t)
                       (define-datatype:report-error 'Type-name?
                         (string-append
                           "did not get a data type bound to an "
                           "  appropriate structure: ~s. "
                           "  This tends to happen when the type name is "
                           "  bound to a lexical variable.")
                         'type-name type-info))))
                 (define-datatype:report-error 'Type-name?
                   (string-append
                     "expects 1 argument, not ~s. "
                     "  With argument list = ~s.")
                    (length args) args))))
           (define-datatype:report-error 'Type-name "Type name is not a symbol: ~s." 'type-name)))
       (define Variant-name
         (let ((expected-length (length '(Field-name ...)))
               (field-names '(Field-name ...))
               (pred-names '(Pred? ...))
               (preds (list (lambda (x) (Pred? x)) ...)))
           (lambda args
             (if (not (= (length args) expected-length))
               (define-datatype:report-error 'Variant-name
                 (string-append
                   "Expected ~s arguments but got ~s arguments."
                   "   Fields are: ~s    Args are: ~s.")
                 expected-length (length args) '(Field-name ...) args))
             (for-each
               (lambda (a f p pname)
                 (if (not (p a))
                   (define-datatype:report-error 'Variant-name "  Bad ~a field (~s ~s) => #f."
                     f pname a)))
               args
               field-names
               preds
               pred-names)
             (cons 'Variant-name args))))
       ...))))
 
(define-syntax cases
  (syntax-rules ()
    ((_ Type-name Expression . Clauses)
     (let ((type-predicate? (isa Type-name)))
       (define-datatype:case-checker
         Type-name
         'Type-name
         'Expression
         'Clauses)
       (let ((x Expression))
         (if (type-predicate? x)
           (define-datatype:case-helper x . Clauses)
           (begin
             (define-datatype:report-error 'cases
               "   Not a ~a variant: ~s." 'Type-name x))))))))

;;; this only works because no-variant datatypes are invalid.
(define-syntax define-datatype:case-helper
  (syntax-rules (else)
    ((_ Variant (else Body0 Body1 ...))
     (begin Body0 Body1 ...))
    ((_ Variant (Purported-variant-name (Purported-field-name ...)
                  Body0 Body1 ...))
     (apply (lambda (Purported-field-name ...) Body0 Body1 ...)
       (cdr Variant)))
    ((_ Variant (Purported-variant-name (Purported-field-name ...)
                  Body0 Body1 ...)
       Clause ...)
     (if (eq? (car Variant) 'Purported-variant-name)
         (apply (lambda (Purported-field-name ...) Body0 Body1 ...)
           (cdr Variant))
         (define-datatype:case-helper Variant Clause ...)))
    ((_ Variant Neither-an-else-nor-clause ...)
     (define-datatype:report-error 'cases
       " Not a ~a clause: ~s." 'Type-name
       (list Neither-an-else-nor-clause ...)))))

;;; ------------------------------
;;; general helpers

(define always?
  (lambda (x) #t))

(define list-of
  (lambda (pred . l)
    (let ((all-preds (cons pred l)))
      (lambda (obj)
        (let loop ((obj obj) (preds '()))
          (or 
            ;; if list is empty, preds should be, too
            (and (null? obj) (null? preds))
            (if (null? preds)
                ;; if preds is empty, but list isn't, then recycle
                (loop obj all-preds)
                ;; otherwise check and element and recur.
                (and (pair? obj)
                     ((car preds) (car obj))
                     (loop (cdr obj) (cdr preds))))))))))
										 
										 


;; Parsed expression datatypes

;Expression types.
;Based on the simple expression grammar, EoPL-2 p6
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
	(var-exp
		(id symbol?))
	; (proc-in-list-exp
		; (id prim-proc?))
	(lambda-exp
		(id check-lam?)
		(body (list-of expression-o?)))
	(set!-exp
		(change expression?)
		(to expression?))
	(multi-lambda-exp
		(id check-lam?)
		(body (list-of expression?)))
	; (namedlet-exp
		; (name symbol?)
		; (id (list-of? list?))
		; (body (list-of? expression)))
	(let-exp
		(vars (list-of symbol?))
		(vals (list-of expression-o?))
		(body (list-of expression?)))
	[let*-exp 
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))]
	[letrec-exp 
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))]
	(if-else-exp 
		(condition expression?)
		(if-true expression?)
		(if-false expression?))
	(if-exp-null
		(condition expression?)
		(if-true expression?))
	(app-exp
		(rator expression-o?)
		(rand (list-of expression-o?)))
	(lit-exp 
		(item lit?)))

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define list-of? 
	(lambda (pred) 
		(lambda (ls)
	; (printf "list-of?\n\n")
	; (display ls)
	(cond
	[(not(list? ls)) (pred? ls)]
	[else
	(or(andmap pred ls) (pred ls))]))))
(define expression-o?
	(lambda (v)
		(or (expression? v) (proc-val? v))))

(define-datatype proc-val proc-val?
	[prim-proc
		(name test-prim?)]
	[lambda-proc
		(exp expression-o?)]
	[lambda-proc-with-env
		(exp proc-val?)
		(env environment?)]
	; [unevaluated-proc
		; (name test-prim?)]
	)
	 
(define test-prim?
	(lambda (x)
		(prim-proc? (list x))))
	 
(define prim-proc?
	(lambda (sym)
		(let loop ([sym sym]
				[prim-procs *prim-proc-names*])
			(cond [(null? prim-procs) #f]
				[(eqv? (car prim-procs) (car sym)) #t]
				[else (loop sym (cdr prim-procs))]))))
	 
;Checks the lambda.	
(define check-lam?
	(lambda (item)
		(or (symbol? item) (null? item) (pair? item) (list? item))))
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
		(syms list?)
		(vals (list-of scheme-value?))
		(env environment?)))
;Rose Reatherford, Assignment 3
;Problem #2

;Sees if the item is a literal.
(define lit?
	(lambda (x)
	;	(printf "lit?\n")
		(cond
		[(quoted? x)  #t]
		[(list? x) #f]
		[(pair? x) (quoted? x)] 
		;[(symbol? x) #f]
		[(number? x)  #t]
		[(string? x) #t]
		[(boolean? x)  #t]
		[(char? x)  #t]
		
		[(vector? x)  #t]
		[else #f]
		)))
		
;Sees if the item is quoted.
(define quoted? 
  (lambda (exp)
  ;	(printf "quoted?\n")
    (and (pair? exp) (eq? (car exp) 'quote))))		

(define parse-exp
	(lambda (datum)
		;(printf "Attempting to parse exp ") (display datum) (newline)
		(cond
			[(symbol? datum)
				; (cond
					; [(prim-proc? datum) (proc-in-list-exp datum)]
					; [else (var-exp datum)])
				(var-exp datum)
			]
			[(lit? datum) (lit-exp datum)]
			[(not (list? datum)) 
				(eopl:error 'parse-exp
					"Error in parse-exp: application ~s is not a proper list" datum)]
			[(pair? datum)
				(cond [(eqv? (car datum) 'set!)
						(if (check-set? (cdr datum))
							(set!-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))
							(eopl:error 'parse-exp 
								"Error in parse-exp: set! expression: ~s" datum))]
					[(eqv? (car datum) 'lambda) 
						(if (check-valid-arg? (cadr datum))
							(lambda-proc (lambda-exp (cadr datum) (map parse-exp (cddr datum))))
							(eopl:error 'parse-exp 
								"Error in parse-exp: lambda expression: ~s" datum))]
					[(eqv? (car datum) 'let)
							(cond
								[(= 1 (length(cdr datum))) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" 'let datum)]
								[(list? (cadr datum))
									(cond
									[(not (andmap list? (cadr datum))) (eopl:error 'parse-exp
										"declarations in ~s-expression not a proper list ~s" 'let datum)]
									[(not (andmap (lambda (x) (eq? 2 (length x))) (cadr datum)))
										(eopl:error 'parse-exp
											"declaration in ~s-exp must be a list of length 2 ~s" 'let datum)]
									[(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
										(eopl:error 'parse-exp
											"vars in ~s-exp must be symbols ~s" 'let datum)]
									[else
										(let* (
										[varvals (cadr datum)]
										[body (cddr datum)]
										[splitls (split varvals)]
										[vars (car splitls)]
										[vals (cadr splitls)]
										)
									(let-exp vars (map parse-exp vals) (map parse-exp body)))])]
								[else 
									(eopl:error 'parse-exp
									"declarations in ~s-expression not a list ~s" 'let datum)])]
					[(eqv? (car datum) 'let*)
						(cond
							[(= 1 (length(cdr datum))) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" 'let* datum)]
							[(not(list? (cadr datum)))
								(eopl:error 'parse-exp
								"declarations in ~s-expression not a list ~s" 'let* datum)]
							[(not (andmap list? (cadr datum))) (eopl:error 'parse-exp
								"declarations in ~s-expression not a proper list ~s" 'let* datum)]
							[(not (andmap (lambda (x) (eq? 2 (length x))) (cadr datum)))
								(eopl:error 'parse-exp
								"declaration in ~s-exp must be a list of length 2 ~s" 'let* datum)]
							[(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
								(eopl:error 'parse-exp
								"vars in ~s-exp must be symbols ~s" 'let* datum)]
							[else
								(let* (
									[varvals (cadr datum)]
									[body (cddr datum)]
									[splitls (split varvals)]
									[vars (car splitls)]
									[vals (cadr splitls)])
									(let*-exp vars (map parse-exp vals) (map parse-exp body)))])]
					[(eqv? (car datum) 'letrec)
						(cond
							[(= 1 (length(cdr datum))) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" 'letrec datum)]
							[(not (list? (cadr datum)))
								(eopl:error 'parse-exp
									"declarations in ~s-expression not a list ~s" 'letrec datum)]
							[(not (andmap list? (cadr datum))) (eopl:error 'parse-exp
								"declarations in ~s-expression not a proper list ~s" 'letrec datum)]
							[(not (andmap (lambda (x) (eq? 2 (length x))) (cadr datum)))
								(eopl:error 'parse-exp
								"declaration in ~s-exp must be a list of length 2 ~s" 'letrec datum)]
							[(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
								(eopl:error 'parse-exp
								"vars in ~s-exp must be symbols ~s" 'letrec datum)]
							[else
								(let* (
									[varvals (cadr datum)]
									[body (cddr datum)]
									[splitls (split varvals)]
									[vars (car splitls)]
									[vals (cadr splitls)])
									(letrec-exp vars (map parse-exp vals) (map parse-exp body)))])]
					; [(eqv? (car datum) 'let)
						; (if (check-let? (cadr datum))
							; (if (null? (cddr datum))
								; (eopl:error 'parse-exp 
									; "Error in parse-exp: let expression: incorrect arguments: ~s" datum)
								; (let-exp (cadr datum) (map parse-exp (cddr datum))))
							; (eopl:error 'parse-exp 
								; "Error in parse-exp: let expression: ~s" datum))]
					; [(eqv? (car datum) 'let*)
						; (if (check-let? (cadr datum))
							; (if (null? (cddr datum))
								; (eopl:error 'parse-exp 
									; "Error in parse-exp: let expression: incorrect arguments: ~s" datum)
								; (let*-exp (cadr datum) (map parse-exp (cddr datum))))
							; (eopl:error 'parse-exp 
								; "Error in parse-exp: let* expression: ~s" datum))]
					; [(eqv? (car datum) 'letrec)
						; (if (check-let? (cadr datum))
							; (if (null? (cddr datum))
								; (eopl:error 'parse-exp 
									; "Error in parse-exp: letrec expression: incorrect arguments: ~s" datum)
								; (letrec-exp (cadr datum) (map parse-exp (cddr datum))))
							; (eopl:error 'parse-exp 
								; "Error in parse-exp: let expression: ~s" datum))]
					[(eqv? (car datum) 'if)
						(if (check-if? datum)
							(if (null? (cdddr datum)) 
								(if-exp-null (parse-exp (cadr datum)) (parse-exp (caddr datum)))
								(if-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum))))
							(eopl:error 'parse-exp
								"Error in parse-exp: if expression: ~s" datum))]
					[else (app-exp
						(parse-exp (car datum))
						(map parse-exp (cdr datum)))])]
			[else (eopl:error 'parse-exp
				"Invalid concrete syntax ~s" datum)])))

(define unparse-exp ; an inverse for parse-exp
  (lambda (exp)
  ;	(printf "unparse-exp\n")
    (cases expression exp
      (var-exp (id) id)
	  ; (proc-in-list-exp (id) id)
      (lambda-exp (id body) 
        (append (list 'lambda id)
          (map unparse-exp body)))
		(multi-lambda-exp (id body)
			(append (list 'lambda id)
			(map unparse-exp body)))
		(let-exp (vars vals body)
				(let* ([unparsevals (map unparse-exp vals)]
				[merged (map (lambda (x y) (list x y)) vars unparsevals)])
				(append (list 'let merged) 
							(map unparse-exp body)))
			)
		(let*-exp (vars vals body)
			(let* ([unparsevals (map unparse-exp vals)]
			[merged (map (lambda (x y) (list x y)) vars unparsevals)])
				(append (list 'let* merged) 
					(map unparse-exp body)))
		)
		(letrec-exp (vars vals body)
			(let* ([unparsevals (map unparse-exp vals)]
			[merged (map (lambda (x y) (list x y)) vars unparsevals)])
				(append (list 'letrec merged) 
					(map unparse-exp body)))
			)
		(app-exp (rator rand)
			(append (list (unparse-exp rator))
			(map unparse-exp rand)))
		(set!-exp (change to)
			(list 'set! (unparse-exp change) 
			(unparse-exp to)))
		(if-else-exp (boolean if-true if-false)
			(list 'if (unparse-exp boolean) (unparse-exp if-true) (unparse-exp if-false)))
		(if-exp-null (boolean if-true)
			(list 'if boolean if-true))
		(lit-exp (item)
			item))))

(define occurs-free? ; in parsed expression
  (lambda (var exp)
  ;	(printf "occurs-free?\n")
    (cases expression exp
      (var-exp (id) (eqv? id var))
      (lambda-exp (id body)
        (and (not (eqv? id var))
             (occurs-free? var body)))
      (app-exp (rator rand)
        (or (occurs-free? var rator)
            (occurs-free? var rand))))))
			
(define check-let?
	(lambda (datum)
		;(printf "check-let?\n")
		(cond [(null? datum) #t]
			[(not (list? datum)) #f]
			[(number? (car datum)) #f]
			[(symbol? (car datum)) (and (equal? (length datum) 2) (expression? (parse-exp (cadr datum))))]
			[else (and (check-let? (cdr datum)) (check-let? (car datum)))])))


		
(define check-valid-arg?
	(lambda (item)
	;	(printf "check-valid-arg?\n")
		(cond [(null? item) #t]
			[(symbol? item) #t]
			[(pair? item) #t]
			[(not (list? item)) #f]
			[else (and (check-valid-arg? (car item)) (check-valid-arg? (cdr item)))])))
			
(define check-if?
	(lambda (datum)
	;	(printf "check-if?\n")
		(cond [(null? (cddr datum)) #f]
			[else #t])))
			
(define check-set?
	(lambda (datum)
	;	(printf "check-set?\n")
		(equal? (length datum) 2)))
(define split 
    (lambda (ls)
	;	(printf "split\n")
      (list (map (lambda (x) (car x)) ls) (map (lambda (y) (cadr y)) ls))))
; (define merge 
	; (lambda (ls1 ls2)
	 ; (map (lambda (x y) (list x y)) ls1 ls2)))
;; This is the file for all our primitive procedures.

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

; +, -, *, /, add1, sub1, zero?, not, = and or < <= >= >
; cons, car, cdr, list, null?,
; assq, eq?, equal?, atom?, length, list->vector, list?, pair?, procedure?, 
; vector->list, vector, make-vector, vector-ref, vector?, number?, symbol?, set-car! , 
; set-cdr!, vector-set! , display , newline
; Add the c**r and c***r 
; procedures (where each "*" stands for an "a" or "d"). 
;

(define apply-prim-proc
	(lambda (prim-proc args)
		;(printf "apply-prim-proc\n")
		(case prim-proc
		    [(+) (apply-all + args 0)]
		    [(-) (apply-switch - + args 0)]
		    [(add1) (+ (car args) 1)]
		    [(sub1) (- (car args) 1)]
		    [(cons) (cons (car args) (cadr args))]
		    [(=) (apply-all-num = args)]
			[(*) (apply-all * args 1)]
			[(/) (apply-all / args 1)]
			
			;; All Prims break after this point, after extensive testing. 
			;; You cannot use the actual procedure itself to do these.
			;; It wil learn as a zero for this assignment.
			;; All need to be re-done.
			[(zero?) (zero?-def (car args))]
			[(not) (not-def (car args))]
			[(and) (and-def args)]
			[(or) (or-def args)]
			[(<) (apply-all-num < args)]
			[(>) (apply-all-num > args)]
			[(<=) (apply-all-num <= args)]
			[(>=) (apply-all-num >= args)]
			[(list) (list-def args)]
			[(null?) (null?-def (car args))]
			[(assq) (assq-def (car args) (cadr args))]
			[(eq?) (eq? (car args) (cadr args))]
			[(equal?) (equal? (car args) (cadr args))]
			[(atom?) (atom?-def (car args))]
			[(length) (length-def (car args))]
			[(list->vector) (list->vector-def (car args))]
			[(list?) (list?-def (car args))]
			[(pair?) (pair? (car args))]
			[(procedure?) (procedure?-def (car args))]
			[(vector->list) (vector->list-def (car args))]
			[(vector) (vector-def args)]
			[(make-vector) (if (length (= 2 (length args))) 
				(make-vector (car args) (cadr args)) 
				(make-vector (car args)))]
			[(vector-ref) (vector-ref-def (car args) (cadr args))]
			[(vector?) (vector? (car args))]
			[(number?) (number? (car args))]
			[(symbol?) (symbol? (car args))]
			[(set-car!) (set-car! (car args) (cadr args))]
			[(set-cdr!) (set-cdr! (car args) (cadr args))]
			[(vector-set!) (vector-set! (car args) (cadr args) (caddr args))]
			[(display) (display (car args))]
			[(newline) (newline)]
			[(caaaar) (car (car (car (car (car args)))))]
			[(caaadr) (car (car (car (cdr (car args)))))]
			[(caaar) (car (car (car (car args))))]
			[(caadar) (car (car (cdr (car (car args)))))]
			[(caaddr) (car (car (cdr (cdr (car args)))))]
			[(caadr) (car (car (cdr (car args))))]
			[(caar) (car (car (car args)))]
			[(cadaar) (car (cdr (car (car (car args)))))]
			[(cadadr) (car (cdr (car (cdr (car args)))))]
			[(cadar) (car (cdr (car (car args))))]
			[(caddar) (car (cdr (cdr (car (car args)))))]
			[(cadddr) (car (cdr (cdr (cdr (car args)))))]
			[(caddr) (car (cdr (cdr (car args))))]
			[(cadr) (car (cdr (car args)))]
			[(car) (car (car args))]
			[(cdaaar) (cdr (car (car (car (car args)))))]
			[(cdaadr) (cdr (car (car (cdr (car args)))))]
			[(cdaar) (cdr (car (car (car args))))]
			[(cdadar) (cdr (car (cdr (car (car args)))))]
			[(cdaddr) (cdr (car (cdr (cdr (car args)))))]
			[(cdadr) (cdr (car (cdr (car args))))]
			[(cdar) (cdr (car (car args)))]
			[(cddaar) (cdr (cdr (car (car (car args)))))]
			[(cddadr) (cdr (cdr (car (cdr (car args)))))]
			[(cddar) (cdr (cdr (car (car args))))]
			[(cdddar) (cdr (cdr (cdr (car (car args)))))]
			[(cddddr) (cdr (cdr (cdr (cdr (car args)))))]
			[(cdddr) (cdr (cdr (cdr (car args))))]
			[(cddr) (cdr (cdr (car args)))]
			[(cdr) (cdr (car args))]
		    [else (error 'apply-prim-proc 
				"Bad primitive procedure name: ~s" 
				prim-proc)])))
				
;For procedures like '-' which much switch between plus and minus.
(define apply-switch
	(lambda (proc1 proc2 args null-value)
		;(printf "apply-switch\n")
			(if (null? args) 
				null-value
				(proc1 (car args) 
					(apply-switch proc2 proc1 (cdr args) null-value)))))
				
(define apply-all 
	(lambda (proc args null-value)
		;(printf "apply-all\n")
		(if (null? args)
			null-value
			(proc (car args) 
				(apply-all proc (cdr args) null-value)))))

(define apply-all-num
	(lambda (proc args)
		;(printf "apply-all-num\n")
		(if (= 2(length args))
			(proc (car args) (cadr args))
			(and (proc (car args) (cadr args)) (apply-all-num proc (cdr args))))))
(define zero?-def
	(lambda (v)
		(equal? v 0)))
		
(define not-def
	(lambda (x) 
	
	(if x #f #t)))

(define and-def
	(lambda (x) 
	(cond 
	[(null? x) #t]
	[(= 1(length x)) (car x)]
	[else
	(if (car args) (and-def (cdr args)) #f)])))
(define or-def
	(lambda (x)
	(cond
	[(null? x) #f]
	[(= 1 (length x)) (car x)]
	[else
		(let ([t (car x)])
			(if t t (or-def (cdr x))))])))
(define null?-def 
	(lambda (x) (eq? x '())))
(define assq-def
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(eq? (caar ls) x) (car ls)]
      [else (assq-def x (cdr ls))])))
(define atom?-def
	(lambda (x)(not (or (pair? x) (null? x)))))
(define length-def
	(lambda (ls)
		(cond
		[(null? ls) 0]
		[else (+ 1(length-def (cdr ls)))]))) 
(define list->vector-def
  (lambda (ls)
    (let ([s (make-vector (length ls))])
      (do ([ls ls (cdr ls)] [i 0 (+ i 1)])
          ((null? ls) s)
        (vector-set! s i (car ls)))))) 
(define list?-def
  (lambda (x)
    (let race ([h x] [t x])
      (if (pair? h)
          (let ([h (cdr h)])
            (if (pair? h)
                (and (not (eq? h t))
                     (race (cdr h) (cdr t)))
                (null? h)))
          (null? h)))))
(define vector->list-def
  (lambda (s)
    (do ([i (- (vector-length s) 1) (- i 1)]
         [ls '() (cons (vector-ref s i) ls)])
        ((< i 0) ls)))) 

(define vector-def
	(lambda (args)
		(letrec ([helper 
			(lambda (n args vec)
				(if (< n (vector-length vec))
					(begin
					(vector-set! vec n (car args))
					(helper (+ 1 n) (cdr args) vec))
					vec))])
			(helper 0 args (make-vector (length args))))))
(define vector-ref-def
	(lambda (vec n)
		(letrec ([helper
			(lambda (currvec n)
				(if (= index n)
					(car currvec)
					(helper  (cdr currvec) (+ 1 index))))])
		(helper vec 0))))
(define list-def
	(lambda (args)
		; (newline)
		; (newline)
		; (printf "\t\t\t")
		; (display args)
		; (newline)
		; (newline)
		(letrec (
			[helper 
				(lambda (args)
					(if (= 1 (length args))
						(cons (car args) '())
						(cons (car args) (helper (cdr args)))))])
		(helper args))))

;(trace apply-prim-proc zero?-def not-def)
(define procedure?-def
	(lambda (args)
		(proc-val? args)
		))
; top-level-eval evaluates a form in the global environment

;;***LIST OF c****r procs*****
; caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
; cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
; cddadr cddar cdddar cddddr cdddr cddr cdr 

(define extend-env
	(lambda (syms vals env)
		;(printf "extend-env\n")
		(extended-env-record syms vals env)))
(define empty-env
	(lambda ()
	
		(empty-env-record)))
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
	;(printf "Beginning evaluation of: ") (display exp) (newline)
    (cases expression exp
		[lit-exp (datum) 
			(if (and (pair? datum) (eqv? (car datum) 'quote))
				(cadr datum)
				datum)]
		; [proc-in-list-exp (id)
			; (unevaluated-proc id)]
		[var-exp (id)
			(apply-env env id
				(lambda (x) x)
				(lambda () (apply-env global-env id
					(lambda (x) x) ;procedure to call if id is in the environment 
					(lambda (x) x))))]
						;(lambda () (begin (eopl:error 'apply-env ; procedure to call if id not in env
							;"variable not found in environment: ~s" id) (newline) (display env))))))]
		[let-exp (vars exp bodies)
			;(printf "I am now in let, evaluating ") (display vars) (newline) ;(printf ", which has the args of ") (display exp) (newline)
			(let ([new-env 
					(extend-env vars 
						(map (lambda (x) 
							(if (and (list? x) (proc-val? x))
								(let ([envir (strike-from-env 
												(cadr (cadr x))
												env)])
									(lambda-proc-with-env x envir))
								(eval-exp x env)))
							exp) env)])
					(let loop ([bodies bodies])
						(if (null? (cdr bodies))
							(eval-exp (car bodies) new-env)
							(begin (eval-exp (car bodies) new-env)
								(loop (cdr bodies))))))]
		[if-else-exp (test-exp then-exp else-exp)
			(if (eval-exp test-exp env)
				(eval-exp then-exp env)
				(eval-exp else-exp env))]
		[if-exp-null (test-exp then-exp)
			(if (eval-exp test-exp env)
				(eval-exp then-exp env))]
		[lambda-exp (params body)
			;(printf "I am now in lambda, evaluating ") (display params) (newline)
			(last (map (lambda (x) 
				(if (expression? x)
					(eval-exp x env)
					(lambda-proc-with-env x env)))
				body))]
		[multi-lambda-exp (param bodies)
			(lambda params
				(let loop ([bodies bodies])
					(if (null? (cdr bodies))
						(eval-exp (car bodies) new-env)
						(begin (eval-exp (car bodies) new-env)
							(loop (cdr bodies))))))]
		[app-exp (rator rands) 
			(let ([proc-value 
						(if (proc-val? rator)
							;(begin (printf "I did NOT evaluate the rator.") (newline)
							rator
							;(begin (printf "I needed to evaluate the rator! ") (newline)
							(eval-exp rator env))]
					[args 
						(if (and (list? rands) (andmap expression? rands))
							(eval-rands rands env)
							rands)])
				;(printf "My proc-value is: ") (display proc-value) (newline) (printf "My args are: ") (display args) (newline) (newline) (newline)
				(apply-proc proc-value args env))]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;Gets the last element in a list.
(define last 
	(lambda (ls)
	;(printf "last\n")
		(cond [(null? (cdr ls)) (car ls)]
			[else (last (cdr ls))])))
		
; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
  	;(printf "eval-rands\n")
    (map (lambda (x) (eval-exp x env))
		rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
	(lambda (proc-value args env)
		;(printf "apply-proc\n")
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[lambda-proc (la) (apply-lambda la args env)]
			[lambda-proc-with-env (la envi) (apply-lambda (cadr la) args envi)]
			; You will add other cases
			[else (error 'apply-proc
                "Attempt to apply bad procedure: ~s" 
                proc-value)])))
				
(define apply-lambda
	(lambda (exp args env)
		;(printf "apply-lambda\t\t")
		; (display exp)
		;(printf "I am now in apply-lambda, looking at ") (display (cadr exp)) (newline)	
		(eval-exp exp
			(if (or (symbol? (cadr exp)) (not (list? (cadr exp))))
					(with-lists (cadr exp) args env)
					;(begin (printf "I am now passing in the values of ") (display (cadr exp)) (printf " with the args of ") (display args) (newline)
					(extend-env 
						(cadr exp)
						args env)))))
						
(define with-lists 
	(lambda (vars args env)
		;(printf "with-lists\n")
		(cond [(symbol? vars) 
				(extend-env (list vars) 
					(list args) env)]
			[(not (list? vars)) 
				(let* ([x-vars (get-nice-vars vars)]
						[x-args (find-correct-args args (get-list-placement vars 0) 0)])
					(extend-env x-vars x-args env))])))
					
(define get-nice-vars
	(lambda (nls)
		;(printf "get-nice-vars\n")
		(cond [(not (pair? nls)) (cons nls '())]
			[else (cons (car nls) (get-nice-vars (cdr nls)))])))
			
(define get-list-placement 
	(lambda (vars count)
		;(printf "get-list-placement\n")
		(cond [(not (pair? vars)) count]
			[else (get-list-placement (cdr vars) (+ 1 count))])))
			
(define find-correct-args
	(lambda (args place count)
		;(printf "find-correct-args\n")
		(cond [(equal? count place) 
				(list args)]
			[else (cons (car args) (find-correct-args (cdr args) place (+ 1 count)))])))

(define *prim-proc-names* 
	'(+ - add1 sub1 cons = * / zero? not and or < > <= >= list null? assq eq? equal? atom? 
	length list->vector list? pair? procedure? vector->list vector make-vector vector-ref 
	vector? number? symbol? set-car! set-cdr! vector-set! display
	caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
	cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
	cddadr cddar cdddar cddddr cdddr cddr cdr))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc      
			*prim-proc-names*)
		(empty-env)))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) 
		; (printf "\t")
		; (display x)
		; (newline)
		; (newline)
		
	(top-level-eval (parse-exp x))))




; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

; (define empty-env
	; (lambda ()
		; (empty-env-record)))
		
(define strike-from-env
	(lambda (var env)
		(strike-from-e var env)))
		
(define strike-from-e
	(lambda (var env)
		(cases environment env
			(empty-env-record () env)
			(extended-env-record (syms vals envi)
				(let ([pos (remove-not-number (map (lambda (x) (list-find-position x syms)) var))])
					(if (andmap number? pos)
						(extended-env-record 
							(strike pos syms 0) (strike pos vals 0)
							(strike-from-e var envi))
						(extended-env-record syms vals
							(extended-env-record syms vals
								(strike-from-e var envi)))))))))
				
(define strike
	(lambda (pos ls count)
		(cond [(null? ls) '()]
			[(ormap (lambda (x) (equal? x count)) pos)
				(strike pos (cdr ls) (+ 1 count))]
			[else (cons (car ls) (strike pos (cdr ls) (+ 1 count)))])))
			
(define remove-not-number
	(lambda (ls)
		(cond [(null? ls) '()]
			[(not (number? (car ls))) (remove-not-number (cdr ls))]
			[else (cons (car ls) (remove-not-number (cdr ls)))])))
			
(define global-env 
	init-env)

; (define extend-env
	; (lambda (syms vals env)
		; (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
	(lambda (env sym succeed fail) 
		(cases environment env
			(empty-env-record ()
				(fail))
			(extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
					(if (number? pos)
						(succeed (list-ref vals pos))
						(apply-env env sym succeed fail)))))))












