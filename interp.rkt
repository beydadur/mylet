#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE
                              ;; -----------------------

                              ((= op 3) (/ num1 num2))
                              (else (- num1 num2))

                              ;; -----------------------
                              
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------

                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (else (cons (- (* num1 num2bot) num2top) num2bot))

                              ;; -----------------------

                              
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------

                              ((= op 3) (cons num1top (* num1bot num2)))
                              (else (cons (- num1top (* num1bot num2)) num1bot))

                              ;; -----------------------
                              
                              ))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------

                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top))) ;; divide
                              (else (cons (- (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot)))

                              ;; -----------------------
                              
                            ))))))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                        ;; -----------------------
                        ;; INSERT YOUR CODE HERE 
                        ;; -----------------------

                        (if (zero? (car num1)) (bool-val #t) (bool-val #f))
                 
                        ;; ----------------------- 
                        ))))

      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------

      (new-list-exp () (list-of-nums-val '()))

      (cons-exp (exp1 exp2)
                (let ((num (expval->num (value-of exp1 env)))
                      (lst (expval->list (value-of exp2 env))))
                  (list-of-nums-val (cons (num-val num) lst))))

      (mul-exp (exp)
                (let ((lst (expval->list (value-of exp env))))
                  (num-val (mul-list-nums lst))))

      (min-exp (exp-list)
         (let ((list-val (map (lambda (exp) (value-of exp env)) exp-list)))
           (if (null? list-val)
               (num-val -1) 
               (num-val (apply min (map expval->rational list-val))))))

      (rational-exp (num1 num2)
                    (if (zero? num2)
                        (eopl:error 'rational-exp "Denominator cannot be zero")
                        (rational-val (cons num1 num2))))

      (simpl-exp (exp1)
                 (let ((num (expval->rational (value-of exp1 env))))
                    (if (number? num) (num-val num)
                        (let* ((numer (car num)) (denom (cdr num))
                               (gcd (gcd-euclid numer denom)))
                              (rational-val (cons (/ numer gcd) (/ denom gcd)))))))

      (if-elif-exp (exp1 exp2 exp3 exp4 exp5)
                    (let ((val1 (value-of exp1 env)))
                      (if (expval->bool val1)
                          (value-of exp2 env)
                          (let loop ((exp3 exp3) (exp4 exp4))
                            (if (null? exp3)
                                (value-of exp5 env)
                                (let ((cond (car exp3))
                                      (exp (car exp4)))
                                  (if (expval->bool (value-of cond env))
                                      (value-of exp env)
                                      (loop (cdr exp3) (cdr exp4)))))))))

      ;; -----------------------
)))
      ;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;

      ;; Multiplate the numbers in a list of ExpVals
      (define (mul-list-nums lst)
        (if (null? lst) 1 (* (expval->num (car lst)) (mul-list-nums (cdr lst)))))

      ;; Euclid's algorithm for finding the greatest common divisor
      (define (gcd-euclid a b) (if (zero? b) a (gcd b (remainder a b))))