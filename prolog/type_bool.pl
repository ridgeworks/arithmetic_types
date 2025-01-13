:- module(type_bool,
	[
	 bool/1,
	 op(200, fy,  not),     % boolean 'not'
	 op(500, yfx, and),     % boolean 'and'
	 op(500, yfx, or)       % boolean 'or'
	]).

/** <module> arithmetic type support for booleans 

This module implements a set of functions on booleans which can be used in standard arithmetic expressions. A boolean value can either be a `0` (representing `false`) or `1` (representing `true`). The only exported predicate is the type check `bool/1`, in addition to the operator definitions for `and`, `or`, and `not`. For the most part the other boolean functions support reification of numeric and atomic term comparisons.

The set of boolean arithmetic functions defined by this module include:
```
:- arithmetic_function(and/2).        % boolean and
:- arithmetic_function(or/2).         % boolean or
:- arithmetic_function(not/1).        % boolean not
:- arithmetic_function(== /2).        % atomic equivalence
:- arithmetic_function(\== /2).       % atomic non-equivalence
:- arithmetic_function(< /2).         % atomic less than
:- arithmetic_function(=< /2).        % atomic less than or equal
:- arithmetic_function(>= /2).        % atomic greater than or equal
:- arithmetic_function(> /2).         % atomic greater than
:- arithmetic_function(between/3).    % atomic between
```

See the ReadMe for this pack for more documentation and examples.
*/

:- use_module(library(arithmetic_types)).
%%:- current_module(arithmetic_types) -> true ; use_module(library(arithmetic_types)).

:- arithmetic_function(and/2).        % boolean and
:- arithmetic_function(or/2).         % boolean or
:- arithmetic_function(not/1).        % boolean not
:- arithmetic_function(== /2).        % atomic equivalence
:- arithmetic_function(\== /2).       % atomic non-equivalence
:- arithmetic_function(< /2).         % atomic less than
:- arithmetic_function(=< /2).        % atomic less than or equal
:- arithmetic_function(>= /2).        % atomic greater than or equal
:- arithmetic_function(> /2).         % atomic greater than
:- arithmetic_function(between/3).    % atomic between

/** 
bool(?X:boolean) is semidet

Succeeds if X is a boolean value (`0` or `1` for `false` or `true` respectively) ; otherwise fails.
*/
%
% type bool, 0 or 1 (representing false or true)
%
bool(B) :- (B==0 ; B==1), !.

%
% Function: logical operators
%
and(B1,B2,R) :- bool(B1), bool(B2), R is B1*B2.

or(B1,B2,R)  :- bool(B1), bool(B2), R is max(B1,B2).

not(B,R)     :- bool(B), R is (B+1) mod 2.

%
% Function: atomic term comparisons
%
==(N1,N2,R)  :- number(N1), number(N2), !, (0 is cmpr(N1,N2) -> R=1 ; R=0).
==(A1,A2,R)  :- atomic(A1), atomic(A2),    (A1  =  A2 -> R=1 ; R=0).

\==(N1,N2,R) :- number(N1), number(N2), !, R is abs(cmpr(N1,N2)).
\==(A1,A2,R) :- atomic(A1), atomic(A2),    (A1  =  A2 -> R=0 ; R=1).

<(N1,N2,R)   :- number(N1), number(N2), !,  (-1 is cmpr(N1,N2) -> R=1 ; R=0).
<(A1,A2,R)   :- atomic(A1), atomic(A2),     (A1  @< A2 -> R=1 ; R=0).
  
=<(N1,N2,R)  :- number(N1), number(N2), !, (1 is cmpr(N1,N2) -> R=0 ; R=1).
=<(A1,A2,R)  :- atomic(A1), atomic(A2),    (A1 @=< A2 -> R=1 ; R=0).
 
>=(N1,N2,R)  :- number(N1), number(N2), !, (-1 is cmpr(N1,N2) -> R=0 ; R=1).
>=(A1,A2,R)  :- atomic(A1), atomic(A2),    (A1 @>= A2 -> R=1 ; R=0).
 
>(N1,N2,R)   :- number(N1), number(N2), !, (1 is cmpr(N1,N2) -> R=1 ; R=0).
>(A1,A2,R)   :- atomic(A1), atomic(A2),    (A1  @> A2 -> R=1 ; R=0).

%
% Function: between
%
between(N1,N2,N,R) :-   % R is (N1=<N) and (N=<N2).
	=<(N1,N,B1),
	=<(N,N2,B2),
	and(B1,B2,R).