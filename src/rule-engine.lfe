#|
@doc
  rule-engine public API
@end
|#

(defmodule rule-engine
  (export (check 1)
          (evaluate 1)))

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

;; TBD

(new-type function-returns-integer)
(new-type function-returns-string)
(new-type function-returns-list)

;; Additional type functions

(defun predicate-or-quantifier? (some-type)
  (or (predicate? some-type)
      (quantifier? some-type)))

;;; API functions

(defun check
  ((some-boolean) (when (or (== some-boolean 'true)
                            (== some-boolean 'false)))
   (type-predicate some-boolean))
  ((some-atom) (when (is_atom some-atom))
   (type-atom some-atom))
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
  (if (predicate-or-quantifier? expression)
    (let ((transformed-expression `(quote ,(transform-expression expression))))
      (let ((evaluated-expression `(eval ,transformed-expression)))
        (rule-engine-log "transformed expression ~p~n" evaluated-expression)
        (eval evaluated-expression)))
    'nil))

;;; Internal functions

(defun check-function
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


(defun transform-expression (expression)
  (cond
   ((or (== expression 'true)
        (== expression 'false))
    `(quote ,expression))
   ((== expression 'or) 'orelse)
   ((== expression 'and) 'andalso)
   ((== expression 'in) 'lists:member)
   ((== expression 'any) 'lists:any)
   ((== expression 'all) 'lists:all)
   ((is_list expression)
    (lists:map #'transform-expression/1 expression))
   ('true expression)))
