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
			[(quotient) (apply-all quotient args 1)]
			[(/) (apply-all / args 1)]
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
			[(eqv?) (eqv? (car args) (cadr args))]
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
						[(map) (map-def (car args) (cdr args))]

			[(apply) 
				(apply-def (car args) (cdr args))]
			[(list-tail) (list-tail (car args) (cadr args))]
			[(append) (apply append-def args)]
			[(reverse) (apply reverse-def args)]
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
	(if (car x) (and-def (cdr x)) #f)])))
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
		(vector-ref vec n)))
(define list-def
	(lambda (args)
		(if (null? args)
			'()
			(cons (car args) (list-def (cdr args))))
		))

(define procedure?-def
	(lambda (args)
		(proc-val? args)
		))

(define map-def
  (lambda (f ls . more)
    (if (null? more)
        (let map1 ([ls ls])
          (if (null? ls)
              '()
              (cons (apply-proc f (car ls))
                    (map1 (cdr ls)))))
        (let map-more ([ls ls] [more more])
          (if (null? ls)
              '()
              (cons
                (apply-proc f (car ls) (map-def car more))
                (map-more (cdr ls) (map-def cdr more))))))))
				
(define apply-def
	(lambda (proc args)
		(apply-proc proc (car args))))
				
(define map-def
	(lambda (proc body)
		(let loop ([item (car body)])
			(if (null? item)
				'()
				(cons (apply-proc proc (list (car item))) (loop (cdr item)))))))

(define append-def
  (lambda args
    (let f ([ls '()] [args args])
      (if (null? args)
          ls
          (let g ([ls ls])
            (if (null? ls)
                (f (car args) (cdr args))
                (cons (car ls) (g (cdr ls))))))))) 
(define reverse-def
	(lambda (lst)
		(letrec ([helper 
		(lambda (forwards backwards)
		(if (null? forwards) backwards 
		(helper (cdr forwards) (cons (car forwards) backwards))))])
		(helper lst '()))))