#lang racket #| CSC324 Fall 2020: Project 2 |#

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide typeof typeo)

;-------------------------------------------------------------------------------
; * Task 1: A Simple Type Inferencer *
;-------------------------------------------------------------------------------

#|
(typeof expr typeenv)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to type

  Returns the type of the expression expr: 'num, 'str, 'bool, 'error
|#
(define (typeof expr typeenv)
  (cond
    ; Constants
    [(number? expr) 'num]
    [(boolean? expr) 'bool]
    [(string? expr) 'str]
    ; Identifiers
    [(symbol? expr) (lookup expr typeenv)]
    ; Builtins    
    ; '((num num) num/bool) / '((str str) str)
    [(and (list? expr)
          (equal? 3 (length expr))
          (set-member? (set '+ '- '* '/ '> '>= '=  '++ ) (first expr)))
     (let ([symbol (first expr)]
           [first-value (typeof (second expr) typeenv)]
           [second-value (typeof (third expr) typeenv)])
       (cond
         [(and (set-member? (set '+ '- '* '/) symbol) (equal? 'num first-value) (equal? 'num second-value)) 'num]
         [(and (set-member? (set '> '>= '=) symbol) (equal? 'num first-value) (equal? 'num second-value)) 'bool]
         [(and (equal? '++ symbol) (equal? 'str first-value) (equal? 'str second-value)) 'str]
         [else 'error]))]
    ;'((bool) bool) / '((num) str) / '((str) num)
    [(and (list? expr)
          (equal? 2 (length expr))
          (set-member? (set '! 'num->string 'len) (first expr)))
     (let ([symbol (first expr)]
           [first-value (second expr)])
       (cond
         [(and (equal? symbol '!) (equal? 'bool first-value)) 'bool]
         [(and (equal? symbol 'num->string) (equal? 'num first-value)) 'str]
         [(and (equal? symbol 'len) (equal? 'str first-value)) 'num]
         [else 'error]))]
    ; Function Calls  
    [(list? expr)
     (let
         ([actual-input (rest expr)];lst contain the rest argument              
          [expect-input (first (lookup (first expr) typeenv))] ;list contains all expected num
          [expect-output (second (lookup (first expr) typeenv))])
           
       (if
        (equal? (map (lambda (x) (typeof x typeenv)) actual-input) expect-input)
        expect-output
        'error))]
    [else 'error] 
    ))

; Helper functions for Task 1

#|
(lookup key alst)
  elem: A key in the association list
  alst: An association list 

  Returns the value corresponding to the first time the key appears in the
  association list, or #f if the key does not exist.

  Examples:
  > (lookup 'b '((a . 3) (b . 4)))
  4
  > (lookup 'b '((a . 3) (b . 4) (b . 5)))
  4
  > (lookup 'c '((a . 3) (b . 4) (b . 5)))
  #f
|#
(define (lookup key alst)
  (cond
    [(empty? alst) #f]
    [(equal? key (car  (first alst)))
     (cdr (first alst))]
    [else (lookup key (rest alst))]))

; Add your helper functions here

;-------------------------------------------------------------------------------
; * Task 2: A Type Inferencer Relation in miniKanren
;-------------------------------------------------------------------------------

#|
(typeo expr typeenv type)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types
  type: The type of the expression

  The relational form of the `typeof` function
|#
(define (typeo expr env type)
  (conde
   ; constants: numbero, stringo, and boolo are miniKanren builtin relations
   ((numbero expr) (== type 'num))
   ((stringo expr) (== type 'str))
   ((boolo expr) (== type 'bool))
   ; identifier: symbolo is a miniKanren builtin relation
   ((symbolo expr) (lookupo expr env type))
   ; builtins
   ((fresh (symbol fir sec fir^ sec^)
           (== (list symbol fir sec) expr)
           (typeo fir env fir^)
           (typeo sec env sec^)
           (conde
            ((membero symbol (list '+ '- '* '/))
             (== fir^ 'num)
             (== sec^ 'num)
             (== type 'num))
            ((membero symbol (list '> '>= '=))
             (== fir^ 'num)
             (== sec^ 'num)
             (== type 'bool))
            ((== symbol '++)
             (== fir^ 'str)
             (== sec^ 'str)
             (== type 'str)))))
   ((fresh (symbol fir fir^)
           (== (list symbol fir) expr)
           (typeo fir env fir^)
           (conde
            ((== symbol '!)
             (== fir^ 'bool)
             (== type 'bool))
            ((== symbol 'num->str)
             (== fir^ 'num)
             (== type 'str))
            ((== symbol 'len)
             (== fir^ 'str)
             (== type 'num)))))
   ; function calls
   ((fresh (fun arg arg-type fun-type arg^)
           (== expr (cons fun arg))
           (lookupo fun env fun-type)
           (type-listo arg env arg-type)
           (== fun-type (list arg^ type))
           (== arg^ arg-type)))
   ; function definitions
   ((fresh (fir rest lam id exp new-env arg-type type^ id-type)
           (== expr (cons fir rest))
           (conde
            ((=/= fir 'lambda)
             (== (list lam id exp) fir)
             (type-listo rest env arg-type)
             (create-env id arg-type env new-env)
             (typeo exp new-env type))
            ((== fir 'lambda)
             (== (list id exp) rest)
             (create-env id '() env new-env)
             (lookup-helper id new-env id-type)
             (== type (list id-type type^)) 
             (typeo exp new-env type^)))))
   ))


; Helper functions for Task 2

#|
(lookupo key alst value)
  elem: A key in the association list
  alst: An association list 
  value: The corresponding value in the association list

  The relational form of the `lookup` function
|#
(define (lookupo key alst value)
  (fresh (fkey fval rest)
         (== (cons (cons fkey fval) rest) alst)
         (conde ((== key fkey)
                 (== value fval))
                ((=/= key fkey)
                 (lookupo key rest value)))))


; Add your helper functions here
#|
(boolo obj)
 obj: boolean
  this function check wether obj is t or f
|#

(define (boolo obj)
  (conde ((== obj #t))
         ((== obj #f))))

#|
(membero elem lst)
 elem: target element
 lst: list
 this function check wether elem in lst.
|#
(define (membero elem lst)
  (fresh (first rest)
         (== lst (cons first rest))
         (conde
          ((== first elem))
          ((membero elem rest)))))
#|
(type-listo expr env list)
 expr: expr
 env: environment
 list: target list
  convert list of elemenr into list of type. 
|#

(define (type-listo expr env list)
  (conde ((== expr '())
          (== list '()))
         ((fresh (first rest type lst)
                 (== expr (cons first rest))
                 (typeo first env type)
                 (== list (cons type lst))
                 (type-listo rest env lst)))))
#|
(create-env id arg env new)
 id:  id 
 arg: argument type
 env: environment
 new: new enbironment
 return a new environment of given id and argument.
|#

(define (create-env id arg env new)
  (conde ((== id '())
          (== new env))
         ((fresh (id-fir id-rest arg-fir arg-rest temp new^ logic)
                 (== id (cons id-fir id-rest))
                 (conde ((=/= arg '())
                         (== arg (cons arg-fir arg-rest))
                         (== temp (cons id-fir arg-fir))
                         )
                        ((== arg '())
                         (== temp (cons id-fir logic))
                         (== arg-rest '())
                         ))
                 (== new (cons temp new^))
                 (create-env id-rest arg-rest env new^)))))

#|
 (lookup-helper id env list)
 id:  identifier 
 env: environment
 list:list
 lookup on a list of identifier.
|#
(define (lookup-helper id env list)
  (conde ((== id '())
          (== list '()))
         ((fresh (id-fir id-rest type list^)
                 (== id (cons id-fir id-rest))
                 (lookupo id-fir env type)
                 (== list (cons type list^))
                 (lookup-helper id-rest env list^)))))