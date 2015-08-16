(module calcmem)

(define (address var env)
   (let loop ((a 0)
	      (e env))
      (if (eq? (car e) var)
	  a
	  (loop (+ a 1) (cdr e)))))

(define (evaluate expression)
   (match-case expression
      ((? number?)
       expression)
      ((+ ?a1 ?a2)
       (+ (evaluate a1) (evaluate a2)))
      ((- ?a1 ?a2)
       (- (evaluate a1) (evaluate a2)))
      ((* ?a1 ?a2)
       (* (evaluate a1) (evaluate a2)))
      ((= ?a1 ?a2)
       (if (= (evaluate a1) (evaluate a2)) 1 0))))

(define (compile expression env next)
   (match-case expression
      ((? symbol?)
       (cons `(LOADVAR ,(address expression env)) next))
      ((let (?x1 ?e1) ?e)
       (compile e1 env
	  (cons 'ADD-ENV
	     (compile e (cons x1 env)
		(cons 'DEL-ENV
		   next)))))
      ((set! ?var ?e)
       (compile e env
	  (cons `(STORE ,(address var env))
	     next)))
      ((begin ?e1 ?e2)
       (compile e1 env
	  (cons 'POP
	     (compile e2 env
		   next))))
      ((? number?)
       (cons `(LOADCTE ,expression) next))
      ((+ ?a1 ?a2)
       (compile a1 env
	  (compile a2 env
	     (cons 'ADD next))))
      ((- ?a1 ?a2)
       (compile a1 env
	  (compile a2 env
	     (cons 'SUB next))))
      ((* ?a1 ?a2)
       (compile a1 env
	  (compile a2 env
	     (cons 'MUL next))))
      ((= ?a1 ?a2)
       (compile a1 env
	  (compile a2 env
	     (cons 'CMP next))))))

(define (set-env! env adr v)
   (if (= adr 0)
       (set-car! env v)
       (set-env! (cdr env) (- adr 1) v)))

(define (exec code stack env)
   (match-case (car code)
      ((LOADVAR ?adr)
       (exec (cdr code) (cons (list-ref env adr) stack) env))
      ((STORE ?adr)
       (exec (cdr code) stack (begin (set-env! env adr (car stack)) env)))
      (ADD-ENV
       (exec (cdr code) (cdr stack) (cons (car stack) env)))
      (DEL-ENV
       (exec (cdr code) stack (cdr env)))
      (POP
       (exec (cdr code) (cdr stack) env))
      ((LOADCTE ?n)
       (exec (cdr code) (cons n stack) env))
      (ADD
       (exec (cdr code) (cons (+ (cadr stack) (car stack)) (cddr stack)) env))
      (SUB
       (exec (cdr code) (cons (- (cadr stack) (car stack)) (cddr stack)) env))
      (MUL
       (exec (cdr code) (cons (* (cadr stack) (car stack)) (cddr stack)) env))
      (CMP
       (exec (cdr code) (cons (if (= (cadr stack) (car stack)) 1 0) (cddr stack)) env))
      (STOP
       (car stack))))

(define e '(let (x 12) (let (y 13) (begin (set! x (- (* (+ 10 97) 72) 13)) x))))
(print e)
(print "Execute = " (exec (print "Compile = " (compile e '() '(STOP))) '() '()))
