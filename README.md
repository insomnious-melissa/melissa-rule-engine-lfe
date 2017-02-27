rule-engine
==========

[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][erlang]

*LFE Rule Engine library*

Rule Engine is a small subset of LISP aimed to handle user input (S-expessions) returning true if expression evaluates to true, false if it evalues to false or nil if it cannot be evaluated. It supports handling surrounding context and supposed to be extensible.

More to be done soon!

## Documentation

See [tests] for usage info:

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

```
$ rebar3 eunit
```

## License

Copyright © 2017, Sergey Sobko

MIT License


[tests]: https://github.com/insomnious-melissa/melissa-rule-engine-lfe/blob/master/test/unit-rule-engine-tests.lfe
[org]: https://github.com/insomnious-melissa
[github]: https://github.com/insomnious-melissa/melissa-rule-engine-lfe
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.2+-blue.svg
[erlang]: https://www.erlang.org/downloads
[erlang badge]: https://img.shields.io/badge/erlang-18+-blue.svg
[github tags]: https://github.com/insomnious-melissa/melissa-rule-engine-lfe/tags
