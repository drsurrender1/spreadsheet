#lang racket #| CSC324 Fall 2020: Project 2 |#

; If you would like us to test this file with our correct implementation of
; "typeof" and "typeo", change the import to "p2-soln.rkt" before submitting
; your code.
(require "p2-base.rkt") ; (require "p2-soln.rkt")
(require "mk.rkt")

; (require racket/pretty) ; you might find the prettyprint function useful

(provide type-check-spreadsheet fill-in)

;-------------------------------------------------------------------------------
; * Task 3: Type Checking a Spreadsheet *
;-------------------------------------------------------------------------------

#|
(type-check-spreadsheet spreadsheet)
  spreadsheet: An spreadsheet AST described in the project handout.

  Returns a list of booleans, representing whether the types of each column
  is correctly annotated.
|#
(define (type-check-spreadsheet spreadsheet)
  (let* ([defs (rest (second spreadsheet))];lst of the def  function with value / add in env/defemv
         [cols (rest (third spreadsheet))] ;lst of columns   1. id type then id with its type\2.computed
         [env (create-env defs '())])
    (check-columns cols env)))
#|
(create-env lst env)
lst: list of defination
env: environment of defination appearing before

Return the eviroment according list of definations' infomation.
|#
(define/match (create-env lst env)
  [('() env) env]
  [((cons x xs) env) (let* ([key (first x)]
                            [value (first (run 1 (out) (typeo (second x) env out)))]
                            [new (cons key value)]
                            [new-env (cons new env)])
                       (create-env xs new-env))])
#|
(check-columns cols env)
cols: list of columns
env: environment of columns and defination appearing before

Check every column in cols, save the right columns type in enviroment, and return a list of bools based on
every column type in cols.
|#
(define/match (check-columns cols env)
  [('() env) '()]
  [((cons x xs) env) (let* ([key (first x)]
                            [value (second x)]
                            [checked-type(if (>= (length (third x)) 2)
                                             (col-value (rest (third x)) value env)
                                             #f)]
                            [new (cons key value)]
                            [new-env (cons new env)])                                                  
                       (if (equal? checked-type #t) (cons checked-type (check-columns xs new-env))
                           (cons checked-type (check-columns xs env))))])

#|
(col-value col value env)
target-value: list of value
value: expected-value type
env: environment of columns and defination appearing before

Return #t or #faccording wether target values are satisfied with the expect value type.
|#
(define/match (col-value target-value value env)
  [('() value env) #t]
  [((cons x xs) value env) (let ([num x]
                                 [tail xs])
                             (cond [(equal? (list value) (run 1 (out) (typeo num env out)))
                                    (col-value tail value env)]                                    
                                   [else #f]))])
;-------------------------------------------------------------------------------
; * Task 4: Synthesizing Programs *
;-------------------------------------------------------------------------------

#|
(fill-in lvar expr type n)
  lvar: The logic variable to be filled in
  expr: An expression following the spreadsheet grammar, with a logic variable
        somewhere in the expression to be filled in.
  type: The desired type of the expression after BLANK is filled in.
  n:    The maximum number of results to return.

  Macro that runs a miniKanren query that will replace the symbol `BLANK`
  in the spreadsheet expression `expr` so that the type of the expression
  is consistent with `type`. The query returns at most `n` results.
|#

(define-syntax fill-in
  (syntax-rules ()
    [(fill-in lvar expr type n)
     (run n (lvar) (typeo expr '() type)) ; TODO
     ]))


