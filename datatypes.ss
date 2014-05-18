;; Parsed expression datatypes

;Expression types.
;Based on the simple expression grammar, EoPL-2 p6
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
	(var-exp
		(id symbol?))
	(set!-exp
		(change symbol?)
		(to expression?))
	[lambda-exp
		(id check-lam?)
		(body (list-of expression-o?))]
	(let-exp
		(vars (list-of symbol?))
		(vals (list-of expression-o?))
		(body (list-of expression?)))
	; (named-let-exp
		; (name symbol?)
		; (vars (list-of symbol?))
		; (vals (list-of expression-o?))
		; (body (list-of expression?)))
	[let*-exp 
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))]
	[letrec-exp 
		(vars (list-of symbol?))
		(idss (list-of expression?))
		(body (list-of expression?))]
	[cond-exp 
		(tests (list-of expression?))
		(vals (list-of expression?))]
	[define-exp
		(name symbol?)
		(body expression?)]
	[begin-exp 
		(items (list-of expression?))]
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
		(item lit?))
	(while-exp
		(test-cond expression?)
		(body (list-of expression?)))
	(or-exp
		(body (list-of expression?)))
	(and-exp
		(body (list-of expression?)))
	(case-exp
		(var expression?)
		(cases (list-of (list-of expression?)))
		(nexts (list-of expression-o?)))
	(unless-exp
		(condition expression?)
		(body (list-of expression?)))
	(when-exp
		(condition expression?)
		(body (list-of expression?)))
	
		
		)

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
		(syms list?)
		(vals list?)
		(env environment?)))

(define list-of? 
	(lambda (pred) 
		(lambda (ls)
			(cond
				[(not(list? ls)) (pred? ls)]
				[else
					(or(andmap pred ls) (pred ls))]))))
	
;Checks if something is an expression or proc-val.
(define expression-o?
	(lambda (v)
		; (newline)
		; (display v)
		; (newline)
		(or (expression? v) (proc-val? v))))
(define element-of?
			(lambda (x ls)
				(cond
				((null? ls)  #f)
				((equal? x (car ls))  #t)
				(else (element-of? x (cdr ls))))))
(define set?
	(lambda (ls)
		(cond 
		((null? ls) #t)
		((not (list? ls)) #f)
		((not (element-of? (car ls) (cdr ls))) (set? (cdr ls)))
		(else #f))))


(define-datatype case-clause case-clause?
	[std-case-clause
		(keys set?)
		(exp (list-of expression?))]
	[else-case-clause
		(exp (list-of expression?))])
		
(define-datatype proc-val proc-val?
	[prim-proc
		(name test-prim?)]
	[lambda-proc-with-env
		(id check-lam?)
		(body (list-of expression-o?))
		(env  environment? )]
	[user-defined 
		(name symbol?)
		(body (list-of expression?))]
	
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

