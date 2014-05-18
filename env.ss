; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define display-env-varvals
	(lambda (env)
		(if (not (or (null? (cdr env)) (null? (cddr env))))
			(begin
		(newline)
		(display (map (lambda (x y) (list x y)) (cadr env) (caddr env)))
		(newline)))))
(define display-env-vars
	(lambda (env)
		(if (not (null? (cdr env)))
			(begin
		(newline)
		(display (cadr env))
		(newline)))))
(define display-env-vals
	(lambda (env)
		(if (not (or (null? (cdr env)) (null? (cddr env))))
			(begin
		(newline)
		(display (caddr env))
		(newline)))))
(define display-env-parent
	(lambda (env)
		(if (not(equal? (car env) (empty-env-record)))
			(begin
		(newline)
		(display (cadddr env))
		(newline)))))
(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms vals env)))
		
(define empty-env
	(lambda ()	
		(empty-env-record)))
(define top-level-eval
  (lambda (form)
	(let ([x (eval-exp form (empty-env))])
    ; later we may add things that are not expressions.
		;(set! global-env init-env)
		x)))
			
(define *prim-proc-names* 
	'(+ - add1 sub1 cons = * quotient / zero? not and or < > <= >= list null? assq eq? equal? atom? 
	length list->vector list? pair? procedure? vector->list vector make-vector vector-ref 
	vector? number? symbol? set-car! set-cdr! vector-set! display newline
	caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
	cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
	cddadr cddar cdddar cddddr cdddr cddr cdr map apply append list-tail eqv?))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc      
			*prim-proc-names*)
		(empty-env)))
			
(define global-env 
	init-env)
(define reset-global-env
	(lambda ()
	(set-car! global-env (car init-env))
	(set-cdr! global-env (cdr init-env))))

(define add-to-global
	(lambda (name body)
		(let* ([proc-names (cadr global-env)]
			[proc-def (caddr global-env)])
		(set-cdr! proc-names (append (list (car proc-names)) (cdr proc-names)))
		(set-car! proc-names name)
		(set-cdr! proc-def (append (list (car proc-def)) (cdr proc-def)))
		(set-car! proc-def body)
		(set! global-env (extended-env-record proc-names proc-def (empty-env))))))
	
; (define new-env
	; (lambda (env)
		; env))
(define list-change
	(lambda (ls old new)
		(letrec (
		[helper
			(lambda(ls)
				(if (null? ls)
					'()
					(if (equal?(car ls) old)
					(append (list new) (cdr ls))
					(append (list (car ls)) (helper (cdr ls))))))])
		(helper ls))))
(define list-change-index
	(lambda (ls index new)
		(let ([maxi (length ls)])
		(letrec (
		[helper
			(lambda (ls ix)
				(if (or(null? ls) (= ix maxi))
					'()
					(if (equal? ix index)
						(append (list new) (cdr ls))
						(append (list (car ls)) (helper (cdr ls) (+ 1 ix))))))])
		(helper ls 0)))))
		
(define env-set!
	(lambda (name body env)
		; (newline)
		; (printf "cadr: ")
		; (display (cadr env))
		
		; (newline)
		; (printf "caddr: ")
		; (display (caddr env))
		; (newline)
		; (printf "cadddr: ")
		; (display (cadddr env))
		(if (not (equal? env (empty-env-record)))
			(begin
				(display "set! --> env change in progress")
				(newline)
				(printf "\t\t")
				(display name)
				(newline)
				(display (cadr env))
				(newline)
				(display (cadddr env))
				(newline)
				(let* ([names (cadr env)]
					[bodies (caddr env)]
					[index (list-find-position name names)])
					; (display index)
					(if (equal? index #f)
						(env-set! name body (cadddr env))
						(let* (	[changed-body (list-change-index bodies index body)]
								[changed-names (list-change-index names index name)])
								(set-cdr! env (list changed-names changed-body (cadddr env)))
								; (display env)
								)))
								
						
			)
			(begin 
				(display "Checking global!")
				(display global-env)
				(let* ([names (cadr global-env)]
						[bodies (caddr global-env)]
						[index (list-find-position name names)])
						(if (equal? index #f)
							(eopl:error 'apply-env ; procedure to call if id not in env
							"variable not found in environment: ~s" name)
							(let* ([changed-body (list-change-index bodies index body)]
									[changed-names (list-change-index names index name)])
									(set-cdr! global-env (list changed-names changed-body (cadddr global-env)))))))
			
			)))
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

