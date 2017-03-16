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

(defmacro rule-engine-macro (macro-name)
  `(defmacro ,macro-name rule-engine-function-and-host-function
     (let* ((rule-engine-function (car rule-engine-function-and-host-function))
            (host-function (cadr rule-engine-function-and-host-function)))
       `#(,rule-engine-function
          (lambda (rawargs env)
            (let* ((args (car (list rawargs)))
                   (arg (car args))
                   (arg1 arg)
                   (vars (lfe_env:get_vars env))
                   (ctx (list `#(args ,args)
                              `#(arg ,arg)
                              `#(arg1 ,arg1)
                              `#(arg2 ,(if (> (length args) 1)
                                         (car (cdr args))
                                         ()))
                              `#(arg3 ,(if (> (length args) 2)
                                         (car (cdr (cdr args)))
                                         ()))))
                   (expanded-ctx (let ((expanded-vars
                                        (lists:keymap
                                         (lambda (val)
                                           (proplists:get_value val
                                                                (maps:to_list vars)
                                                                val))
                                         2
                                         ctx)))
                                   (++ expanded-vars
                                       (lists:filtermap
                                        (lambda (value)
                                          (let ((`#(,key ,val) value))
                                            (if (orelse
                                                 (lists:member key
                                                               (++ (list 'true
                                                                         'false
                                                                         'macros)
                                                                   (proplists:get_keys
                                                                    expanded-vars))))
                                              'false
                                              `#(true ,value))))
                                        (maps:to_list vars)))))
                   (expanded-arg (proplists:get_value 'arg expanded-ctx arg))
                   (full-ctx (++ expanded-ctx
                                 (if (is_list expanded-arg)
                                   (if (> (length expanded-arg) 0)
                                     (let* ((quoted
                                             (eval `(if (== (car (quote ,expanded-arg))
                                                            'quote)
                                                      ,expanded-arg
                                                      (quote ,expanded-arg)))))
                                       (list `#(quoted ,quoted)
                                             `#(unquoted ,quoted))))
                                   ())))
                   (rules (maps:to_list
                           (maps:filter (lambda (key val)
                                          (lists:member key
                                                        additional-macros))
                                        (lfe_env:get_funs env)))))
              (if (== ',',macro-name 'bind)
                (rule-engine:evaluate '`(,',host-function ,@args) full-ctx rules)
                (rule-engine:evaluate ',host-function full-ctx rules))))))))

(rule-engine-macro bind)
(rule-engine-macro rule)

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
        (bind count erlang:length)
        (bind eq erlang:==)
        (bind gt erlang:>)
        (bind lt erlang:<)
        (bind valid? rule-engine:check)))

(defun rule-engine-rules ()
  (list (rule identity arg)
        (rule quoted? (eq (first arg) 'quote))
        (rule unquote unquoted)
        (rule second (first (rest quoted)))
        (rule third (first (rest (rest quoted))))
        (rule bool? (in arg '(true false
                              'true 'false
                              ''true ''false)))
        (rule null? (and (list? arg)
                         (eq (count arg) 0)))
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
                                   `#(macros ,(proplists:get_keys rule-engine))
                                   `#(additional-macros ,(proplists:get_keys additional-rules)))
                                  additional-variables)
                              env-with-macros)))
    (rule-engine-log "eval-vars ~p~n" additional-variables)
    (rule-engine-log "eval-expr ~p~n" expression)
    (eval expression env-with-variables)))

(defun check (expression)
  (rule-engine:check expression 'expr?))

(defun check (expression validator)
  (rule-engine:evaluate `(,validator expression)
                        (list `#(expression ,expression))
                        (list (rule function?
                                    (and (list? expression)
                                         (not (string? expression))
                                         (not (null? expression))
                                         (in (first expression) macros)))

                              (rule oneargument?
                                    (and (valid? expression 'function?)
                                         (eq (count (rest expression)) 1)))
                              (rule manyarguments?
                                    (and (valid? expression 'function?)
                                         (gt (count (rest expression)) 1)))

                              (rule not-statement?
                                    (and (valid? expression 'oneargument?)
                                         (eq (first expression) 'not)
                                         (valid? (second expression) 'boolexpr?)))
                              (rule and-or-statement?
                                    (and (valid? expression 'manyarguments?)
                                         (in (first expression) '(or and))))

                              (rule boolexpr?
                                    (or (bool? expression)
                                        (and (valid? expression 'function?)
                                             (or (valid? expression 'and-or-statement?)
                                                 (valid? expression 'not-statement?)))))

                              (rule expr?
                                    (valid? expression 'boolexpr?)))))
