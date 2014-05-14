(define apply-prim-proc
	(lambda (prim-proc args cont)
		(case prim-proc
		    [(+) (apply-all + args 0 cont)]
		    [(-) (apply-switch - + args 0 cont )]
		    [(add1) (cont (+ (car args) 1))]
		    [(sub1) (cont (- (car args) 1))]
		    [(cons) (cont(cons (car args) (cadr args)))]
		    [(=) (apply-all-num = args cont)]
			[(*) (apply-all * args 1 cont)]
			[(quotient) (apply-all quotient args 1 cont)]
			[(/) (apply-all / args 1 cont)]
			[(zero?) (zero?-def (car args) cont)]
			[(not) (not-def (car args) cont)]
			[(and) (and-def args cont)]
			[(or) (or-def args cont)]
			[(<) (apply-all-num < args cont)]
			[(>) (apply-all-num > args cont)]
			[(<=) (apply-all-num <= args cont)]
			[(>=) (apply-all-num >= args cont)]
			[(list) (list-def args cont)]
			[(null?) (null?-def (car args) cont)]
			[(assq) (assq-def (car args) (cadr args) cont)]
			[(eq?) (cont(eq? (car args) (cadr args)))]
			[(equal?) (cont(equal? (car args) (cadr args)))]
			[(atom?) (atom?-def (car args) cont)]
			[(length) (length-def (car args) cont)]
			[(list->vector) (list->vector-def (car args))]
			[(list?) (list?-def (car args) cont)]
			[(pair?) (cont(pair? (car args)))]
			[(procedure?) (procedure?-def (car args))]
			[(vector->list) (vector->list-def (car args))]
			[(vector) (vector-def args)]
			[(make-vector) (if (length (= 2 (length args))) 
				(make-vector (car args) (cadr args)) 
				(make-vector (car args)))]
			[(vector-ref) (vector-ref-def (car args) (cadr args))]
			[(vector?) (cont(vector? (car args)))]
			[(number?) (cont(number? (car args)))]
			[(symbol?) (cont(symbol? (car args)))]
			[(set-car!) (set-car! (car args) (cadr args))]
			[(set-cdr!) (set-cdr! (car args) (cadr args))]
			[(vector-set!) (vector-set! (car args) (cadr args) (caddr args))]
			[(display) (display (car args))]
			[(newline) (newline)]
			[(caaaar) (cont (car (car (car (car (car args))))))]
			[(caaadr) (cont(car (car (car (cdr (car args))))))]
			[(caaar) (cont(car (car (car (car args))))]
			[(caadar) (cont(car (car (cdr (car (car args)))))]
			[(caaddr) (cont(car (car (cdr (cdr (car args)))))]
			[(caadr) (cont(car (car (cdr (car args))))]
			[(caar) (cont(car (car (car args)))]
			[(cadaar) (cont(car (cdr (car (car (car args)))))]
			[(cadadr) (cont(car (cdr (car (cdr (car args)))))]
			[(cadar) (cont(car (cdr (car (car args))))]
			[(caddar) (cont(car (cdr (cdr (car (car args)))))]
			[(cadddr) (cont(car (cdr (cdr (cdr (car args)))))]
			[(caddr) (cont(car (cdr (cdr (car args))))]
			[(cadr) (cont(car (cdr (car args)))]
			[(car) (cont(car (car args))]
			[(cdaaar) (cont(cdr (car (car (car (car args)))))]
			[(cdaadr) (cont(cdr (car (car (cdr (car args)))))]
			[(cdaar) (cont(cdr (car (car (car args))))]
			[(cdadar) (cont(cdr (car (cdr (car (car args)))))]
			[(cdaddr) (cont(cdr (car (cdr (cdr (car args)))))]
			[(cdadr) (cont(cdr (car (cdr (car args))))]
			[(cdar) (cont(cdr (car (car args)))]
			[(cddaar) (cont(cdr (cdr (car (car (car args)))))]
			[(cddadr) (cont(cdr (cdr (car (cdr (car args)))))]
			[(cddar) (cont(cdr (cdr (car (car args))))]
			[(cdddar) (cont(cdr (cdr (cdr (car (car args)))))]
			[(cddddr) (cont(cdr (cdr (cdr (cdr (car args)))))]
			[(cdddr) (cont(cdr (cdr (cdr (car args))))]
			[(cddr) (cont(cdr (cdr (car args)))]
			[(cdr) (cont(cdr (car args))]
						[(map) (map-def (car args) (cdr args))]
			; [(apply) (display 'apply)(display (cadr args))(apply-def (car args) (cdr args))]

			[(apply) 
				; (display 'apply)(display (cadr args))
				(apply-def (car args) (cdr args))]
		    [else (error 'apply-prim-proc 
				"Bad primitive procedure name: ~s" 
				prim-proc)])))
(define apply-switch
	(lambda (proc1 proc2 args null-value cont)
		(if (null? args) 
			(cont null-value)
			(apply-switch proc2 proc1 (cdr args) null-value 				
				(lambda (x) 		
					(cont (proc1 (car args) x))
				)
			)
		)
	)
)
(define apply-all 
	(lambda (proc args null-value cont)
		(if (null? args)
			(cont null-value) 
			(apply-all proc (cdr args) null-value (lambda (x) (cont (proc (car args) x)))) 
		)
	)
)
(define apply-all-num
	(lambda (proc args cont)
		(length-cps args 
			(lambda (x)
				(if (= 2 x)
					(cont (proc (car args) (cadr args)))
					(apply-all-num proc (cdr args) 
						(lambda (y)
							(cont (and (proc (car args) (cadr args)) y))
						)
					)
				)
			)
		)
	)
)
		
(define zero?-def
	(lambda (v cont)
		(cont(equal? v 0))))
		
(define not-def
	(lambda (x cont) 
	
	(cont(if x #f #t))))

(define length-cps
	(lambda (ls k)
		(if (null? ls)
			(k 0)
			(length-cps (cdr ls) 
				(lambda (x)
					(k(+ 1 x)))))))
(define and-def
	(lambda (x cont) 
	(cond 
	[(null? x) (cont #t)]
	[else 
		(length-cps x 
			(lambda (res)
				(if (= 1 res) 
					(cont (car x))
					(if (car args)
						(and-def (cdr args) cont)
						(cont #f)
					)
				)
			)
		)
	]
	)
	)
)
(define or-def
	(lambda (x cont)
	(cond
	[(null? x) (cont #f)]
	[else
		(length-cps x 
			(lambda (res)
				(if (= 1 res)
					(cont (car x))
					(if (car x)
						(cont (car x))
						(or-def (cdr x) cont)
					)
				)
			)
		)
	]
	)
	)
)
(define null?-def 
	(lambda (x cont) (cont(eq? x '()))))
(define assq-def
  (lambda (x ls cont)
    (cond
      [(null? ls) (cont #f)]
      [(eq? (caar ls) x) (cont(car ls))]
      [else (assq-def x (cdr ls) cont)])))
(define atom?-def
	(lambda (x cont)(cont(not (or (pair? x) (null? x))))))
(define length-def
	(lambda (ls cont)
		(cond
		[(null? ls) (cont 0)]
		[else
			(length-def (cdr ls)
				(lambda (x)
					(+ 1 x)
				)
			)
		]
		)
	)
)
(define list->vector-def
  (lambda (ls)
    (let ([s (make-vector (length ls))])
      (do ([ls ls (cdr ls)] [i 0 (+ i 1)])
          ((null? ls) s)
        (vector-set! s i (car ls)))))) 
(define list?-def
	(lambda (x k)
		(if (pair? x)
			(if (pair? (cdr x))
				(list?-def (cddr x)
					(lambda (v)
						(k (and (not (eq? (cdr x) x)) v))))
				(k (null? (cdr x))))
			(k (null? x)))))
(define vector->list-def
  (lambda (s)
    (do ([i (- (vector-length s) 1) (- i 1)]
         [ls '() (cons (vector-ref s i) ls)])
        ((< i 0) ls)))) 

(define vector-def
	(lambda (args)
		(letrec ([helper 
			(lambda (n args vec)
				; (display args)
				; (newline)
				(if (< n (vector-length vec))
					(begin
					(vector-set! vec n (car args))
					(helper (+ 1 n) (cdr args) vec))
					vec))])
			(helper 0 args (make-vector (length args))))))
(define vector-ref-def
	(lambda (vec n)
		(vector-ref vec n)))

(define list-def
	(lambda (args cont)
		(if (null? args)
			(cont '())
			(list-def (cdr args) 
				(lambda (x)
					(cons (car args) x)
				)
			)
		)
	)
)
(define procedure?-def
	(lambda (args)
		(proc-val? args)
		))
(define apply-def
	(lambda (proc args)
		(apply-proc proc (car args))))
				
(define map-def
	(lambda (proc body)
		(let loop ([item (car body)])
			(if (null? item)
				'()
				(cons (apply-proc proc (list (car item))) (loop (cdr item)))))))

					