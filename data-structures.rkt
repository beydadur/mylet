#lang eopl

;; data structures for let-lang.

(provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  
  ;; -----------------------
  ;; INSERT YOUR CODE HERE 
  ;; -----------------------

  (rational-val
   (rational pair?))
  (list-of-nums-val
   (lst list?))

  ;;add this for tests.rkt
  (str-val
    (str string?))
  ;; -----------------------
)

;;; extractors:

;; expval->num : ExpVal -> Int
;; Page: 70
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; -----------------------
;; INSERT YOUR CODE HERE 
;; -----------------------

;; expval->list : ExpVal -> List
(define expval->list
  (lambda (v)
    (cases expval v
      (list-of-nums-val (lst) lst)
      (else (expval-extractor-error 'list-of-nums-val v)))))

(define expval->rational
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (rational-val (rat) rat)
      (else (expval-extractor-error 'rational v)))))
    
;; -----------------------

;; expval->bool : ExpVal -> Bool
;; Page: 70
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

(define empty-env-record
  (lambda () 
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-env-record? null?)

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))
