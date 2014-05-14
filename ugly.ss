


;; Parsed expression datatypes

;Expression types.
;Based on the simple expression grammar, EoPL-2 p6
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
	(var-exp
		(id symbol?))
	(lambda-exp
		(id (list-of check-lam?))
		(body (list-of expression?)))
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
		(vals (list-of expression?))
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
		(rator expression?)
		(rand (list-of expression?)))
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
	[(not(list? ls)) (printf "not a list\n")(pred? ls)]
	[else
	(or(andmap pred ls) (pred ls))]))))
(define expression-o?
	(lambda (v)
		(or (expression? v) (proc-val? v))))

(define-datatype proc-val proc-val?
	[prim-proc
		(name test-prim?)]
	[lambda-proc
		(exp expression-o?)])
	 
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

;(load "chez-init.ss") ; remove this isf using Dr. Scheme EoPL language

;(load "chez-init.ss") ; remove this isf using Dr. Scheme EoPL language



(define lit?
	(lambda (x)
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
(define quoted? 
  (lambda (exp)
    (and (pair? exp) (eq? (car exp) 'quote))))		
		
(define check-lam?
	(lambda (item)
		(or (symbol? item) (null? item))))

(define parse-exp
	(lambda (datum)
		(cond
			[(symbol? datum) (var-exp datum)]
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
						(if (check-lambda? datum)
							(if (pair? (cadr datum))  
								(lambda-exp (cadr datum) (map parse-exp (cddr datum)))
								(multi-lambda-exp (cadr datum) (map parse-exp (cddr datum))))
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
    (cases expression exp
      (var-exp (id) id)
      (lambda-exp (id body) 
        (append (list 'lambda id)
          (map unparse-exp body)))
		(multi-lambda-exp (id body)
			(append (list 'lambda id)
			(map unparse-exp body)))
		(let-exp (vars vals body)
				(let ([merged (merge vars (map unparse-exp vals))])
				(append (list 'let merged) 
							(map unparse-exp body)))
			)
		(let*-exp (vars vals body)
			(let ([merged (merge vars (map unparse-exp vals))])
				(append (list 'let* merged) 
					(map unparse-exp body)))
		)
		(letrec-exp (vars vals body)
			(let ([merged (merge vars (map unparse-exp vals))])
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
		(cond [(null? datum) #t]
			[(not (list? datum)) #f]
			[(number? (car datum)) #f]
			[(symbol? (car datum)) (and (equal? (length datum) 2) (expression? (parse-exp (cadr datum))))]
			[else (and (check-let? (cdr datum)) (check-let? (car datum)))])))

(define check-lambda?
	(lambda (datum)
		(and  (check-valid-arg? (cadr datum))
			(cond [(null? (cddr datum)) #f]
				[else #t]))))
		
(define check-valid-arg?
	(lambda (item)
		(cond [(null? item) #t]
			[(symbol? item) #t]
			[(not (list? item)) #f]
			[else (and (check-valid-arg? (car item)) (check-valid-arg? (cdr item)))])))
			
(define check-if?
	(lambda (datum)
		(cond [(null? (cddr datum)) #f]
			[else #t])))
			
(define check-set?
	(lambda (datum)
		(equal? (length datum) 2)))
(define split 
    (lambda (ls)
      (list (map (lambda (x) (car x)) ls) (map (lambda (y) (cadr y)) ls))))
(define merge 
	(lambda (ls1 ls2)
	 (map (lambda (x y) (list x y)) ls1 ls2)))
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
			[(procedure?) (procedure? (car args))]
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
			[(quote) (quote (car args))]
		    [else (error 'apply-prim-proc 
				"Bad primitive procedure name: ~s" 
				prim-proc)])))
				
;For procedures like '-' which much switch between plus and minus.
(define apply-switch
	(lambda (proc1 proc2 args null-value)
			(if (null? args) 
				null-value
				(proc1 (car args) 
					(apply-switch proc2 proc1 (cdr args) null-value)))))
				
(define apply-all 
	(lambda (proc args null-value)
		(if (null? args)
			null-value
			(proc (car args) 
				(apply-all proc (cdr args) null-value)))))

(define apply-all-num
	(lambda (proc args)
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
		(letrec (
			[helper 
				(lambda (args)
					(if (= 1 (length args))
						(cons (car args) '())
						(cons (car args) (helper (cdr args)))))])
		(helper args))))

;(trace apply-prim-proc zero?-def not-def)
; top-level-eval evaluates a form in the global environment

;;***LIST OF c****r procs*****
; caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
; cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
; cddadr cddar cdddar cddddr cdddr cddr cdr 
(define extend-env
	(lambda (syms vals env)
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
    (cases expression exp
		[lit-exp (datum) 
			(if (and (pair? datum) (eqv? (car datum) 'quote))
				(cadr datum)
				datum)]
		[var-exp (id)
			(apply-env env id
				(lambda (x) x)
				(lambda () (apply-env global-env id
					(lambda (x) x) ;procedure to call if id is in the environment 
						(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
							"variable not found in environment: ~s" id)))))]
		[let-exp (vars exp bodies)
			(let ([new-env 
					(extend-env vars 
						(map (lambda (x) (eval-exp x env)) exp) env)])
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
			(last (map (lambda (x) 
				(if (expression? x)
					(eval-exp x env)
					x))
				body))]
		[multi-lambda-exp (param bodies)
			(lambda params
				(let loop ([bodies bodies])
					(if (null? (cdr bodies))
						(eval-exp (car bodies) new-env)
						(begin (eval-exp (car bodies) new-env)
							(loop (cdr bodies))))))]
		[app-exp (rator rands) 
			(let ([proc-value rator]
					[args 
						(if (andmap expression? rands)
							(eval-rands rands env)
							rands)])
				(if (proc-val? proc-value)
					(apply-proc proc-value args env)
					(apply-proc (eval-exp proc-value env) args env)))]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;Gets the last element in a list.
(define last 
	(lambda (ls)
		(cond [(null? (cdr ls)) (car ls)]
			[else (last (cdr ls))])))
		
; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env))
		rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
	(lambda (proc-value args env)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[lambda-proc (la) (apply-lambda la args env)]
			; You will add other cases
			[else (error 'apply-proc
                "Attempt to apply bad procedure: ~s" 
                proc-value)])))
				
(define apply-lambda
	(lambda (exp args env)
		(eval-exp exp
			(extend-env 
				(cadr exp)
				args env))))

(define *prim-proc-names* 
	'(+ - add1 sub1 cons = * / zero? not and or < > <= >= list null? assq eq? equal? atom? 
	length list->vector list? pair? procedure? vector->list vector make-vector vector-ref 
	vector? number? symbol? set-car! set-cdr! vector-set! display
	caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
	cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
	cddadr cddar cdddar cddddr cdddr cddr cdr quote))

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
		;(display x)
		;(newline)
	(top-level-eval (parse-exp x))))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3


		
(define global-env 
	init-env)


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






; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
	(lambda ()
		(empty-env-record)))
		
(define global-env 
	init-env)

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms vals env)))

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


						





