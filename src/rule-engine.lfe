#|
@doc
  rule-engine public API
@end
|#

(defmodule rule-engine
  (export (check 1)
          (evaluate 1)
          (evaluate 2)))

;;; Macros

(defmacro rule-engine-log args
  (let ((format (car args))
        (params (cdr args)))
    `(funcall #'io:format/2 (++ "[RE] " ,format) (list ,@params))))

(defmacro new-type (type)
  (let* ((type-name (atom_to_list type))
         (type-function (list_to_atom (++ "type-" type-name)))
         (type-param (list_to_atom (++ "some-" type-name)))
         (check-function (list_to_atom (++ type-name "?"))))
    `(progn
       (defun ,type-function (,type-param)
         (rule-engine-log "~p is of type ~p~n" ,type-param ,type-name)
         ',type)

       (defun ,check-function (some-type)
         (rule-engine-log "is ~p of type ~p?~n" some-type ',type)
         (== (check some-type) ',type)))))

;;; Atom types

(new-type atom)
(new-type integer)
(new-type string)
(new-type list)
(new-type null)
(new-type quantifier)
(new-type predicate)

(new-type ctx-bool-var)
(new-type ctx-string-var)
(new-type ctx-integer-var)
(new-type ctx-list-var)

;; TBD

(new-type function-returns-integer)
(new-type function-returns-string)
(new-type function-returns-list)

;; Additional type functions

(defun inherit?
  (('ctx-bool-var 'predicate) 'true)
  (('ctx-string-var 'string) 'true)
  (('ctx-integer-var 'integer) 'true)
  (('ctx-list-var 'list) 'true)
  ((_some-other-type _any-base-type) 'false))

(defun predicate-or-quantifier? (some-type)
  (or (predicate? some-type)
      (quantifier? some-type)))

(defun of-the-same-type? (arguments)
  (== (lists:map #'check/1 arguments)))

(defun startswith (string prefix)
  (=:= (string:str string prefix) 1))

(defun ctx-get-in (variable ctx)
  (fletrec ((get-in ((() current-map) current-map)
                    ((path current-map)
                     (get-in (cdr path)
                             (maps:get (list_to_atom (car path))
                                       current-map ())))))
    (get-in (cdr (string:tokens (atom_to_list variable) "."))
            ctx)))

;;; API functions

(defun check
  ((some-boolean) (when (or (== some-boolean 'true)
                            (== some-boolean 'false)))
   (type-predicate some-boolean))
  ((some-atom) (when (is_atom some-atom))
   (let ((atom-list (atom_to_list some-atom)))
     (cond
      ((startswith atom-list "ctx.bool-") (type-ctx-bool-var some-atom))
      ((startswith atom-list "ctx.str-") (type-ctx-string-var some-atom))
      ((startswith atom-list "ctx.int-") (type-ctx-integer-var some-atom))
      ((startswith atom-list "ctx.list-") (type-ctx-list-var some-atom))
      ('true (type-atom some-atom)))))
  ((some-integer) (when (is_integer some-integer)) ;; Integer
   (type-integer some-integer))
  ((some-list) (when (is_list some-list)) ;; List or String
   (cond
     ((== (length some-list) 0) (type-null some-list))
     ((io_lib:printable_unicode_list some-list) (type-string some-list))
     ('true (let (((cons function params) some-list))
              (rule-engine-log "trying to handle ~p as function~n" some-list)
              (check-function function params))))))

(defun evaluate (expression)
  (evaluate expression ()))

(defun evaluate (expression ctx)
  (if (predicate-or-quantifier? expression)
    (let* ((external-functions #m(ctx-get-in #'ctx-get-in/2))
           (transformed-expression `(quote ,(transform-expression expression
                                                                 ctx
                                                                 external-functions
                                                                 ()))))
      (let ((evaluated-expression `(eval ,transformed-expression)))
        (rule-engine-log "transformed expression ~p~n" evaluated-expression)
        (eval evaluated-expression)))
    'nil))

;;; Internal functions

(defun check-function
  (('count (list list-expr)) (andalso (orelse (list? list-expr)
                                              (string? list-expr)
                                              (null? list-expr))
                                      (type-integer `(count ,list-expr))))
  (('first (list list-expr)) (andalso (orelse (list? list-expr)
                                              (string? list-expr)
                                              (null? list-expr))
                                      (type-integer `(first ,list-expr))))
  (('eq arguments) (andalso (of-the-same-type? arguments)
                            (type-predicate `(eq ,@arguments))))
  (('gt arguments) (andalso (of-the-same-type? arguments)
                            (type-predicate `(gt ,@arguments))))
  (('lt arguments) (andalso (of-the-same-type? arguments)
                            (type-predicate `(lt ,@arguments))))
  (('gte arguments) (andalso (of-the-same-type? arguments)
                             (type-predicate `(gte ,@arguments))))
  (('lte arguments) (andalso (of-the-same-type? arguments)
                             (type-predicate `(lte ,@arguments))))
  (('in (list what where)) (andalso (or (integer? what)
                                        (string? what))
                                    (or (list? where)
                                        (null? where))
                                    (type-predicate `(in (,what ,where)))))
  (('and arguments) (andalso (lists:all #'predicate-or-quantifier?/1 arguments)
                             (type-predicate `(and ,@arguments))))
  (('or arguments) (andalso (lists:all #'predicate-or-quantifier?/1 arguments)
                            (type-predicate `(or ,@arguments))))
  (('not (list predicate)) (andalso (predicate-or-quantifier? predicate)
                                    (type-predicate `(not ,predicate))))
  (('all (list predicate arguments)) (andalso (predicate? predicate)
                                              (list? arguments)
                                              (type-quantifier `(all ,predicate ,arguments))))
  (('any (list predicate arguments)) (andalso (predicate? predicate)
                                              (list? arguments)
                                              (type-quantifier `(any ,predicate ,arguments))))
  ((_unknown _params) 'list))

(defmacro external-function (function-name)
  `(,function-name (maps:get ',function-name external-functions)))

(defun transform-expression (expression ctx external-functions outer-expression)
  (let ((external-function 'ctx-get-in))
    (maps:get expression #m(or orelse
                            and andalso
                            in lists:member
                            any lists:any
                            all lists:all
                            eq ==
                            gt >
                            lt <
                            gte >=
                            lte =<
                            count length
                            first car
                            second cadr)
              (cond
               ((or (== expression 'true)
                    (== expression 'false))
                `(quote ,expression))
               ((andalso (is_list expression)
                         (not (string? expression)))
                (if (andalso (of-the-same-type? expression)
                             (string? (car expression)))
                  `(quote ,expression)
                  (lists:map (lambda (inner-expression)
                               (transform-expression inner-expression
                                                     ctx
                                                     external-functions
                                                     expression))
                             expression)))
               ((andalso (is_atom expression)
                         (startswith (atom_to_list expression) "ctx."))
                (ctx-get-in expression ctx))
               ('true expression)))))
