; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014
(cd "C:/Users/daveyle/Desktop/CSSE304/Interpreter/20131430/")
(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
	(load "interpreter.ss")
	(load "primitives.ss")))
	(load "primitives.ss")
    (load "interpreter.ss")))


(load-all)

(define l load-all) ; even easier!
