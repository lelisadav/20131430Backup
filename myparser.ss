(define split 
    (lambda (ls)
      (list (map (lambda (x) (car x)) ls) (map (lambda (y) (cadr y)) ls))))
(define merge 
	(lambda (ls1 ls2)
	 (map (lambda (x y) (list x y)) ls1 ls2)))
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
  (var-exp
	;(printf "var: \n")
    (id symbol?))
  (lambda-exp
	;(printf "lambda: \n")
    (id pair?)
    (body (list-of? expression?)))
  (thunk-exp
	(id null?)
	(body (list-of? expression?)))
  (multi-lambda-exp
	;(printf "multi-lambda: \n")
	(id symbol?)
	(body (list-of? expression?)))
  (app-exp
    (rator expression?)
    (rand (list-of? expression?)))
  (if-exp
	(condition expression?)
	(truebody expression?))
  (if-else-exp
	(condition expression?)
	(truebody expression?)
	(falsebody expression?))
  (namedlet-exp
	(name symbol?)
	(vars (list-of? symbol?))
	(vals (list-of? expression?))
	(body (list-of? expression?)))
  (let-exp
	(vars (list-of? symbol?))
	(vals (list-of? expression?))
	(body (list-of? expression?)))
  (let*-exp 
	(vars (list-of? symbol?))
	(vals (list-of? expression?))
	(body (list-of? expression?)))
  (letrec-exp 
	(vars (list-of? symbol?))
	(vals (list-of? expression?))
	(body (list-of? expression?)))
  (set!-exp
	(var symbol?)
	(expr expression?))
 (lit-exp
    (id literal?))
	 )
(define literal?
	(lambda (x)
		(cond
		[(list? x) #f]
		[(pair? x) (quoted? x)] 
		;[(symbol? x) #f]
		[(number? x)  #t]
		[(string? x) #t]
		[(boolean? x)  #t]
		[(char? x)  #t]
		[(quoted? x)  #t]
		[(vector? x)  #t]
		[else #f]
		)))
(define quoted? 
  (lambda (exp)
    (and (pair? exp) (eq? (car exp) 'quote))))
; (define letvarvals?
	; (lambda (ls)
		; (let ([split (split-lists ls '() '())])
		; (and (list-of? symbol? (car split)) (list-of? expression? (cadr split))))))
(define list-of? 
	(lambda (pred) 
		(lambda (ls)
	; (printf "list-of?\n\n")
	; (display ls)
	(cond
	[(not(list? ls)) (printf "not a list\n")(pred? ls)]
	[else
	(or(andmap pred ls) (pred ls))]))))

(define parse-exp
  (lambda (datum)
    (cond
	  
      ((symbol? datum) 	(var-exp datum))
	  ((literal? datum) (lit-exp datum))
      ((list? datum)
       (cond
	   [(eqv? (car datum) 'lambda)
			(cond
			
			[(< (length datum) 3) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
			[(null? (cadr datum))
				(thunk-exp (cadr datum) (map parse-exp (cddr datum)))]
			[(not (list? (cadr datum)))
					(multi-lambda-exp (cadr datum) (map parse-exp (cddr datum)))]
			[else 
				(cond
					[(not((list-of? symbol?) (cadr datum))) 
						(eopl:error 'parse-exp
						"lambda's formal arguments ~s must all be symbols" (cadr datum))]
					[else
				(lambda-exp (cadr datum) 
				(map parse-exp (cddr datum)))])])]
	   [(eqv? (car datum) 'if)
			(cond 
			[(and(not(= 2 (length (cdr datum)))) (not (= 3 (length (cdr datum)))))
				(eopl:error 'parse-exp
             "if-expression ~s does not have (only) test, then, and else" datum)]
			 [else
			(let ([condition (cadr datum)]
				[body (cddr datum)])
			(cond
			[(= 2 (length (cdr datum)))
				(if-exp (parse-exp condition) (parse-exp (car body)))]
			[(= 3 (length (cdr datum)))
				(if-else-exp (parse-exp condition) (parse-exp (car body)) (parse-exp (cadr body)))]
			[else (eopl:error 'parse-exp
             "if-expression ~s does not have (only) test, then, and else" datum)]))])]
		[(eqv? (car datum) 'let)
			(cond
			[(= 1 (length(cdr datum))) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" 'let datum)]
			[(symbol? (cadr datum)) ;named let
				(cond
				[(not (list? (caddr datum)))(eopl:error 'parse-exp
					"declarations in ~s-expression not a list ~s" 'named-let datum)]
				[(not (andmap list? (caddr datum))) (eopl:error 'parse-exp
					"declarations in ~s-expression not a proper list ~s" 'named-let datum)]
				[(not (andmap (lambda (x) (eq? 2 (length x))) (caddr datum)))
				(eopl:error 'parse-exp
					"declaration in ~s-exp must be a list of length 2 ~s" 'named-let datum)]
				[(not (andmap (lambda (x) (symbol? (car x))) (caddr datum)))
					(eopl:error 'parse-exp
					"vars in ~s-exp must be symbols ~s" 'named-let datum)]
				[else 
					(let* ([name (cadr datum)]
					[varvals (caddr datum)]
					[body (cdddr datum)]
					[splitls (split varvals)]
					[vars (car splitls)]
					[vals (cadr splitls)])
				(namedlet-exp name vars (map parse-exp vals) (map parse-exp body)))])]
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
		[(eqv? (car datum) 'set!)
			(cond
			[(not (= 2 (length (cdr datum)))) 
				(eopl:error 'parse-exp
					"set! expression has incorrect length ~s" datum)]
			[else
				(let* ([var (cadr datum)]
					[expr (cddr datum)])
					(set!-exp var (parse-exp expr)))])]
		[else 
		; (printf "app-exp ")
		; (display datum)
		; (printf "\n car datum ")
		; (display (car datum))
		; (printf "\nparse: ")
		; (display (map parse-exp (reverse (cdr datum))))
		; (printf "\n\n")
		(app-exp(parse-exp (car datum))(map parse-exp (cdr datum)))]))
	  ((pair? datum) 
		(eopl:error 'parse-exp
              "expression ~s is not a proper list" datum))
	  
	  
      (else (eopl:error 'parse-exp
              "Invalid concrete syntax ~s" datum)))
	
	)
)

(define unparse-exp ; an inverse for parse-exp
	(lambda (exp)
	;(printf "\n\n\texp: ")
	;(display exp)
		(cases expression exp
			(var-exp (id) id)
			(multi-lambda-exp (id body)
				(append (list 'lambda id) (map unparse-exp body)))
			(thunk-exp (id body)
				(append (list 'lambda '()) (map unparse-exp body)))
			(lambda-exp (id body) 
				(append (list 'lambda id)
				(map unparse-exp body)))
			(if-exp (condition truebody)
				(list 'if (unparse-exp condition) (unparse-exp truebody))
			)
			(if-else-exp (condition truebody falsebody)
				(list 'if (unparse-exp condition) (unparse-exp truebody) (unparse-exp falsebody))
			)
			(namedlet-exp (name vars vals body)
				(let ([merged (merge vars (map unparse-exp vals))])
				(append (list 'let name merged) 
							(map unparse-exp body)))
			)
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
			(set!-exp (var expr)
				(list 'set! var (unparse-exp expr))
			)
			(lit-exp (id)
				id
			)
			(app-exp (rator rand)
				(append (list (unparse-exp rator))
				(map unparse-exp rand))
			)
		)
	)
)




; (define let->application
	; (lambda (ls)
	
	; (let* ( (varval (split-lists (cadr ls) '() '()))
			; (vars (car varval))
			; (vals (cadr varval)))
		; (printf "\nvarval: ")
		; (display varval)
		; (printf "\nvars: ")
		; (display vars)
		; (printf "\nvals: ")
		; (display vals)
		; (cons (list 'lambda vars (caddr ls)) vals)
	
	; )
	; )
	; )

; (define named-let->letrec
	; (lambda (ls)
		; (let* (
		; [name (cadr ls)]
		; [varval (split-lists (caddr ls) '() '())]
		; [vars (car varval)]
		; [vals (cadr varval)]
		; [body (cdddr ls)])
		; (list(list 'letrec (list(list name (append (list 'lambda vars) body)))
   ; name)
 ; vals))))

; (define merge-lists
			; (lambda (currlist vars vals)
				; (printf "\ncurrlist: ")
				; (display currlist)
				; (printf "\nvars: ")
				; (display vars)
				; (printf "\nvals: ")
				; (display vals)
				; (cond 
				; ((or(null? vars)(null? vals)) currlist)
				; (else 
				; (merge-lists (append currlist (list (list (car vars) (car vals)))) (cdr vars) (cdr vals))
				; ))))

; (define application->let
	; (lambda (ls)
	; (let ([e (cdr ls)]
		; [b (cddar ls)]
		; [x (cadar ls)]) 
	 ; (list 'let (list(merge-lists '() x e)) b)
	; )))
; (define let->let*
	; (lambda (ls)
	; (letrec ([let*s
		; (lambda (lets val)
			; (cond
			; ((null? val) lets)
			; (else (let*s (append lets (car val) ) (cdr val)))))])
	; (list 'let* (let*s '() (cdr ls))))))
; (define let*->let
	; (lambda (ls)
		; (letrec ([nestedlet
			; (lambda (lets val)
				; (cond
				; ((null? lets) val)
				; (else (list 'let (list (car lets)) (nestedlet (cdr lets) val)))))])
			; (append (nestedlet (cadr ls) (caddr ls))))))