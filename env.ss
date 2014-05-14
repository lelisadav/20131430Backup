; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

;?????????Can you explain the difference between strike-from-env and strike-from-e?
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
;This function changes a variable in an environment
(define change-env
	(lambda (var cha env)
		(cases environment env
			(empty-env-record () env)
			(extended-env-record (syms vals envi)
				(let ([pos (list-find-position var syms)])
					(if (number? pos)
						(extend-env (cons var (remove-at-pos pos syms 0)) 
							(cons cha (remove-at-pos pos (vector->list vals) 0)) envi)
						(extended-env-record syms vals
								(change-env var cha envi))))))))
								
(define check-in-env?
	(lambda (id env)
		(cases environment env
			(empty-env-record () #f)
			(extended-env-record (syms vals envi)
				(let ([pos (remove-not-number (map (lambda (x) (list-find-position x syms)) id))])
					(if (andmap number? pos)
						#t
						(check-in-env? id envi)))))))
						
(define go-through-and-change
	(lambda (ids args env)
		(if (null? (cdr ids))
			env
			(go-through-and-change (cdr ids) (cdr args) 
				(change-env (car ids) (car args) env)))))
								
(define remove-at-pos
	(lambda (pos ls count)
		(if (equal? pos count)
			(cdr ls)
			(cons (car ls) (remove-at-pos pos (cdr ls) (+ 1 count))))))
			
(define get-place
	(lambda (cha pos count)
		(cond [(equal? pos count) (car cha)]
			[else (get-place (cdr cha) pos (+ 1 count))])))

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
			
(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (list->vector vals) env)))
		
(define empty-env
	(lambda ()	
		(empty-env-record)))
(define top-level-eval
  (lambda (form)
	(let ([x (eval-exp form (empty-env))])
    ; later we may add things that are not expressions.
		(set! global-env init-env)
		x)))
			
(define *prim-proc-names* 
	'(+ - add1 sub1 cons = * quotient / zero? not and or < > <= >= list null? assq eq? equal? atom? 
	length list->vector list? pair? procedure? vector->list vector make-vector vector-ref 
	vector? number? symbol? set-car! set-cdr! vector-set! display
	caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
	cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
	cddadr cddar cdddar cddddr cdddr cddr cdr map apply))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc      
			*prim-proc-names*)
		(empty-env)))
			
(define global-env 
	init-env)

(define new-env
	(lambda (env)
		env))
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
		 
(define extend-env-recursively 
	(lambda (vars idss vals old-env)
		(let ([len (length vars)])
			(let ([vec (make-vector len)])
				(let ([env (extended-env-record vars vec old-env)])
					(for-each
						(lambda (pos ids body)
							(vector-set! vec pos (lambda-proc-with-env ids (list body) env)))
						
						(iotass len 0) idss vals) env)))))
; (define for-each-redef
  ; (lambda (f ls . more)
    ; (do ([ls ls (cdr ls)] [more more (map cdr more)])
        ; ((null? ls))
      ; (apply f (car ls) (map car more))))) 
;(define make-range
;	(lambda (m n)
;		(if (>= m n) 
;		'()
;		(cons m (make-range (+ m 1) n)))))
				
(define iotass
	(lambda (pos count)
		(if (equal? (+ 1 count) pos)
			(list count)
			(cons count (iotass pos (+ 1 count))))))

(define apply-env
	(lambda (env sym succeed fail) 
		(cases environment env
			(empty-env-record ()
				(fail))
			(extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
					(if (number? pos)
						(succeed (vector-ref vals pos))
						(apply-env env sym succeed fail)))))))

