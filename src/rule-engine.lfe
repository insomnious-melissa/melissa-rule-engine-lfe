#|
@doc
  rule-engine public API
@end
|#

(defmodule rule-engine
  (export (check 1)
          (check 2)
          (evaluate 1)
          (evaluate 2)
          (evaluate 3)))

;;; Base macros

(defmacro rule-engine-log args
  (let ((format (car args))
        (params (cdr args)))
    `(funcall #'io:format/2 (++ "[RE] " ,format) (list ,@params))))

(defmacro bind rule-engine-function-and-host-function
  (let* ((rule-engine-function (car rule-engine-function-and-host-function))
         (host-function (cdr rule-engine-function-and-host-function)))
    `#(,rule-engine-function (lambda (args env)
                               `(let ((env ,env))
                                  (,',@host-function ,@args))))))

(defmacro rule rule-engine-function-and-body
  (let* ((rule-engine-function (car rule-engine-function-and-body))
         (body (cdr rule-engine-function-and-body)))
    (rule-engine-log "~p ~p~n" rule-engine-function body)
    `#(,rule-engine-function (lambda (rawargs env)
                               (let* ((args (car (list rawargs)))
                                      (arg (car args))
                                      (arg1 arg)
                                      (arg2 (if (> (length args) 1)
                                              (car (cdr args))
                                              ()))
                                      (arg3 (if (> (length args) 2)
                                              (car (cdr (cdr args)))
                                              ())))

                                 (log "~p ~p~n" (list ',rule-engine-function args))
                                 ,@body)))))

;;; Rule Engine

(defun rule-engine-macros ()
  (list (bind in lists:member)
        (bind and lfe:andalso)
        (bind or lfe:orelse)
        (bind not erlang:not)
        (bind atom? erlang:is_atom)
        (bind integer? erlang:is_integer)
        (bind string? io_lib:printable_unicode_list)
        (bind list? erlang:is_list)
        (bind first cl:car)
        (bind rest cl:cdr)
        (bind log io:format)
        (bind length erlang:length)
        (bind eq erlang:==)
        (bind gt erlang:>)
        (bind lt erlang:<)
        (bind valid? rule-engine:check)))

(defun rule-engine-rules ()
  (list (rule identity arg)
        (rule quoted? `(eq (first ',arg) 'quote))
        (rule vars (lfe_env:get_vars env))
        (rule unquote `(if (eq (first ',arg) 'quote)
                         ,arg
                         ',arg))
        (rule apply `(,arg ,@(rest args)))
        (rule second `(first (rest (unquote ,arg))))
        (rule third `(first (rest (rest (unquote ,arg)))))
        (rule bool? `(in ,arg '(true false 'true 'false)))
        (rule null? `(and (list? ,arg)
                          (eq (length ,arg) 0)))
        (rule macros? (in arg macros))
        (rule any `(or ,@(lc ((<- val arg2))
                           `(,arg1 ,val))))
        (rule all `(and ,@(lc ((<- val arg2))
                            `(,arg1 ,val))))))

(defun evaluate (expression)
  (rule-engine:evaluate expression () ()))

(defun evaluate (expression additional-variables)
  (rule-engine:evaluate expression additional-variables ()))

(defun evaluate (expression additional-variables additional-rules)
  (let* ((rule-engine-macros (rule-engine-macros))
         (rule-engine-rules (rule-engine-rules))
         (rule-engine (++ rule-engine-macros
                          rule-engine-rules
                          additional-rules))

         (new-env (lfe_env:new))
         (env-with-macros (lfe_env:add_mbindings rule-engine new-env))
         (env-with-variables (lfe_env:add_vbindings
                              (++ (list #(true true)
                                        #(false false)
                                        `#(macros ,(proplists:get_keys rule-engine)))
                                  additional-variables)
                              env-with-macros)))
    (eval expression env-with-variables)))

(defun check (expression)
  (rule-engine:check expression 'expr?))

(defun check (expression validator)
  (rule-engine:evaluate `(,validator expression)
                        (list `#(expression ,expression))
                        (list (rule function?
                                    `(and (list? ,arg)
                                          (not (string? ,arg))
                                          (not (null? ,arg))
                                          (in (first ,arg) macros)))

                              (rule rboolexpr? `(valid? ,arg 'boolexpr?))
                              (rule allrboolexpr? `(all rboolexpr? ,arg))

                              (rule boolexpr?
                                    `(or (bool? ,arg)
                                         (and (function? ,arg)
                                              (or (and (in (first ,arg) '(or and))
                                                       (gt (length (rest ,arg)) 1))
                                                  (and (eq (first ,arg) 'not)
                                                       (eq (length (rest ,arg)) 1)
                                                       (rboolexpr? (first (rest ,arg))))))))

                              (rule expr?
                                    `(boolexpr? ,arg)))))
