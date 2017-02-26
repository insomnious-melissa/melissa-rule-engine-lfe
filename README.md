rule-engine
==========

LFE Rule Engine library.

Rule Engine is a small subset of LISP aimed to handle user input (S-expessions) returning true if expression evaluates to true, false if it evalues to false or nil if it cannot be evaluated.

More to be done soon!

## Examples

Using LFE:

```lfe
;; Check if S-expression conforms Rule Engine

(== (rule-engine:check '(or true false)) 'predicate)
(== (rule-engine:check '(in true or)) 'false)

;; Evaluate S-expression using Rule Engine
(== (rule-engine:evaluate '(or true false)) 'true)
(== (rule-engine:evaluate '(and true false (in 3 (1 2 3)) 'false)))
(== (rule-engine:evaluate '(and true false or)) 'nil)
```

The same using Erlang:

```erlang
% Check if S-expression conforms Rule Engine
'rule-engine':check(['or','true','false']) == predicate.
'rule-engine':check(['in','true','or']) == false.

% Evaluate S-expression using Rule Engine
'rule-engine':evaluate(['or','true','false']) == true.
'rule-engine':evaluate(['and','true','false',['in',3,[1,2,3]]]) == false.
'rule-engine':evaluate(['and','true','false','or']) == nil.
```

## Build

```
$ rebar3 compile
```

## Test

Not done yet
