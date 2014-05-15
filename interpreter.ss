; top-level-eval evaluates a form in the global environment

;;***LIST OF c****r procs*****
; caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
; cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
; cddadr cddar cdddar cddddr cdddr cddr cdr 

; eval-exp is the main component of the interpreter

(print-graph #t)
(define list-fill
	(lambda (num obj)
		(if (zero? num)
			'()
			(cons obj (list-fill (- num 1) obj)))))
; This takes an environment and an expression and evaluates the expr in the env
(define eval-exp
  (lambda (exp env)
    (cases expression exp
		[lit-exp (datum) 
			(if (and (pair? datum) (eqv? (car datum) 'quote))
				(cadr datum) ;this is because we don't want it to deal with quote
				datum)]
		[var-exp (id) ;this takes a variable exp, applies the environment to it and returns the result
			(apply-env env id
				(lambda (x) x)
				(lambda () (apply-env global-env id
					(lambda (x) x) ;procedure to call if id is in the environment 
					(lambda () (display env) (begin (eopl:error 'apply-env ; procedure to call if id not in env
							"variable not found in environment: ~s" id) (newline) (display env))))))]
		[let-exp (vars exp bodies) ;this is a stub
			(printf "I shouldn't be here, ever!")]
		[named-let-exp (name vars exp bodies) ;this is a stub
			(printf "Something went wrong.")]
		[letrec-exp (vars vals bodies) 
		;this creates a ls of the length vars, and fills it with evaluated expressions, then puts that in the env.
			(let* ([ls (list-fill(length vars) '#(42))]
				[new-env (extend-env vars ls env)]
				[evals (map (lambda (x) (eval-exp x new-env)) vals)])
				(set-car! ls (car evals))
				(set-cdr! ls (cdr evals))
					(let loop ((bodies bodies))
						(if (null? (cdr bodies))
							(eval-exp (car bodies) new-env)
							(begin 
								(eval-exp (car bodies) new-env)
								(loop (cdr bodies))))))]
			
			; (car (map (lambda (x) (eval-exp x (extend-env-recursively vars idss vals env))) body))]
		[lambda-exp (id body)
			;this converts a lambda-exp to a lambda-proc, which is necessary, because the environment of a lambda-proc 
			;is not known until it evaluated and that is not done until here, as opposed to being in parse
			(lambda-proc-with-env id body env)]
		[if-else-exp (test-exp then-exp else-exp)
			;this creates a 2-armed if by evaluating all 3 parts of the expr
			(if (eval-exp test-exp env)
				(eval-exp then-exp env)
				(eval-exp else-exp env))]
		[if-exp-null (test-exp then-exp)
			;this is a 1-armed if
			(if (eval-exp test-exp env)
				(eval-exp then-exp env))]
		[app-exp (rator rands) 
			;this is an application of a procedure to a set of arguments
			;first it returns the proc-value for the rator by evaling the rator
			;the it evaluates the rands
			;finally it applies the evaluated rator to the evaluated rands
			(let* ([proc-value (eval-exp rator env)]
					[args (eval-rands rands env)])
				(apply-proc proc-value args))]
		[case-exp (var cases body)
			;this is a stub
			(printf "I should never be here!")]
		[set!-exp (id body)
			(printf "Nothing here.")]
		[while-exp (test body)
			;this creates a while-exp by first checking to see if the test is true
			;if so it iterates through the body and 
			;evaluates each line of the body
			(if (eval-exp test env)
				(begin (loop-through body env)
					(eval-exp exp env)))]
		[unless-exp (condition body)
			(printf "Error! unless-exp")]
		[when-exp (condition body)
			;this is also a stub
			(printf "Error when-exp")]
				
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))
		


;Gets the last element in a list.
(define last 
	(lambda (ls)
		(cond [(null? (cdr ls)) (car ls)]
			[else (last (cdr ls))])))
			
(define loop-through
	;this function loops through a list of expr and evaluates them in the specified env
	(lambda (ls env)
		(if (null? (cdr ls)) 
			(eval-exp (car ls) env)
			(begin (eval-exp (car ls) env)
				(loop-through (cdr ls) env)))))
		
; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (x)
	(eval-exp x env))
		rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[lambda-proc-with-env (id body envi) (apply-lambda id body args envi)]
			[else (error 'apply-proc
                "Attempt to apply bad procedure: ~s" 
                proc-value)])))
				
;Evaluates the lambda.
(define apply-lambda
	(lambda (id body args env)
		(let ([envi 
			(if (or (symbol? id) (not (list? id)))
				(with-lists id args env)
				(extend-env 
					id
					args env))])
			(loop-through body envi))))
				
;???????????Can you give me a quick run-down on the next four functions? Just a sentence or two would be great.				
(define with-lists 
	(lambda (vars args env)
		(cond [(symbol? vars) 
				(extend-env (list vars) 
					(list args) env)]
					
					
					
			[(not (list? vars)) 
				(let* ([x-vars (get-nice-vars vars)]
						[x-args (find-correct-args args (get-list-placement vars 0) 0)])
					(extend-env x-vars x-args env))])))
					
(define get-nice-vars
	(lambda (nls)
		(cond [(not (pair? nls)) (cons nls '())]
			[else (cons (car nls) (get-nice-vars (cdr nls)))])))
			
(define get-list-placement 
	(lambda (vars count)
		(cond [(not (pair? vars)) count]
			[else (get-list-placement (cdr vars) (+ 1 count))])))
			
(define find-correct-args
	(lambda (args place count)
		(cond [(equal? count place) 
				(list args)]
			[else (cons (car args) (find-correct-args (cdr args) place (+ 1 count)))])))



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
	;This is used to evaluate one expression. I know right?
	;It first parses the expression
	;Then it runs it through syntax-expand
	;Then it evaluates it in the top level
		; (newline)
		; (newline)
		; (printf "\tEvaluating:\t")
		; (display x)
		; (newline)
		; (printf "\tThe correct answer is:\t")
		 (top-level-eval (syntax-expand (parse-exp x)))
		; (let ((res (eval x)))
			; (display res)
			; (newline)
			; (display "\tOur result: ")
			; (let ((ourres 
			; (top-level-eval (syntax-expand (parse-exp x)))
			; ))
			; (display ourres)
			; (newline)
			; (if (equal? ourres res)
				; (display "\tCorrect!")
				; (display "\tIncorrect.")
				; )
			; ourres))
			)
			
			)

(define syntax-expand
	(lambda (datum)
		(cases expression datum
			[var-exp (id) (var-exp id)] ;no changes needed
			[lit-exp (id) (lit-exp id)] ;no changed needed
			[lambda-exp (id body) 
				(lambda-exp id
					(map syntax-expand body))] ;just have to check for changes in the body
			[let-exp (vars vals body) 
				;converts the let-exp to an application of lambda, as shown by the syntax-definition of let
				(app-exp (lambda-exp vars (map syntax-expand body)) (map syntax-expand vals))]
					
			[let*-exp (vars vals body)
				;converts a let*-exp into an application of lambda as shown by the syntax-definition of let*
				(if (null? vars)
					(map syntax-expand body)
					(app-exp 
						(lambda-exp (list (car vars)) 
							(syntax-expand 
								(let*-exp (cdr vars) (cdr vals) body))) 
						(list (car vals))))]
			[letrec-exp (vars vals body)
				;creates a letrec expression
				; (display idss)
				(letrec-exp vars (map syntax-expand vals) (map syntax-expand body))]
			; ((letrec ((name (lambda (var ...) body1 body2 ...)))
					; name)
			; expr ...)
			[named-let-exp (name vars vals body)
				;creates a named-let-exp
				;!!!!!!!!!!!!!!!!!!!Work in Progress!
				; (display vars)
				; (newline)
				; (display vals)
				(app-exp 
					(syntax-expand
						(letrec-exp (list name) (list vars) 
							(list (lambda-exp vars (map syntax-expand body)))
							(list(var-exp name)))) (map syntax-expand vals))] 
			[define-exp (name body)
				;allows the use of define
				(lambda-exp (list name) (list (syntax-expand body)))]
			[cond-exp (tests vals)
				;creates nested if statements
				(cond [(and (null? tests) (not (null? vals)))
						(if-exp-null (parse-exp '(lambda () #t)) (syntax-expand (car vals)))]
					[(and (null? (cdr tests)) (not (null? (cdr vals))))
						(if-else-exp (syntax-expand (car tests)) (syntax-expand (car vals)) (syntax-expand (cadr vals)))]
					[(and (null? (cdr tests)) (null? (cdr vals)))
						(if-exp-null (syntax-expand (car tests)) (syntax-expand (car vals)))]
					[else 
						(if-else-exp (syntax-expand (car tests)) (syntax-expand (car vals))
							(syntax-expand (cond-exp (cdr tests) (cdr vals))))])]
			
			[begin-exp (items)
				;creates begin, which is basically putting your thingies into a thunk and using that thunk.
				(app-exp (lambda-exp '() 
					(map syntax-expand items)) '())]
			[app-exp (rator rands)
				;just have to check for changes to rator and rand
				(app-exp (syntax-expand rator) (map syntax-expand rands))]
			[set!-exp (id body)
				;just have to check for changes to the body
				(set!-exp id (syntax-expand body))]
			[if-else-exp (test success fail)
				;just have to check for changes to the success and fail expr
				(if-else-exp test 
					(syntax-expand success) (syntax-expand fail))]
			[if-exp-null (test success)
				;just have to check for changes to the success expr
				(if-exp-null test (syntax-expand success))]
			[while-exp (test body)
				;checks for changes to the test and the bodies
				(while-exp (syntax-expand test) (map syntax-expand body))]
			[or-exp (body) 
				;converts the or-exp to the parsed form of this: [basically]
					; (define or-def
						; (lambda (x)
							; (cond
							; [(null? x) #f]
							; [(= 1 (length x)) (car x)]
							; [else
							; (if (car x) (car x) (or-def (cdr x)))])))
				(if (null? body)
					(lit-exp '#f)
					(app-exp (lambda-exp (list 'v)
						(list (if-else-exp (var-exp 'v)
							(var-exp 'v)
							(syntax-expand (or-exp (cdr body)))))) (list (car body))))]
			[and-exp (body) 
				;converts the and-exp to the parsed form of this: [basically]
				; (define and-def
					; (lambda (x) 
						; (cond 
							; [(null? x) #t]
							; [(= 1(length x)) (car x)]
							; [else
								; (if (car x) (and-def (cdr x)) #f)])))
				(if (null? body)
					(lit-exp '#t)
					(app-exp (lambda-exp (list 'v)
						(list (if-else-exp (var-exp 'v)
							(syntax-expand (and-exp (cdr body)))
							(var-exp 'v)))) (list (car body))))]
			[case-exp (var cases next)
				;converts to a conditional expression that does or
				;????????????Right?
				(syntax-expand (cond-exp (map (lambda (x) (change-to-or var x)) cases) (map syntax-expand next)))]
			[unless-exp (condition body)
				;this is basically the parsed form of this:
					;(if (not condition)
					;	(begin body))
				(if-exp-null (app-exp (var-exp not) (syntax-expand condition)) (map syntax-expand body))]
			[when-exp (condition body)
				;this is basically the parsed form of this:
					;(if condition
					;	(begin body))
				(if-exp-null (syntax-expand condition) (map syntax-expand body))]
				
									
						
						; Error here with it not correctly going onto the next list of statements. Needs better if-exp-null
						; integration! Something like doing a cond exp of all the cond expressions? Or a multi-bodied lambda would probably work better
						; as long as it short-circuited. It will get the correct answer if it's odd! But nothing for anything else... XD
						)))
	
		
(define change-to-or
	(lambda (var ls)
		(syntax-expand 
			(or-exp (map (lambda (x) (app-exp (var-exp 'equal?) (list var x))) ls)))))


		










