#### Introduction
Prolog has provided functional arithmetic since the very early days, and support for user defined functions was often provided via predicates which had the result as the last argument. When SWI-Prolog introduced optimized arithmetic, this capability was dropped, but a partial solution was provided using `library(arithmetic)`:
```
/** <module> Extensible arithmetic

This module provides a  portable   partial  replacement  of SWI-Prolog's
user-defined  arithmetic  (evaluable)   functions.    It   defines   the
compatibility  directive  arithmetic_function/1  and  support  for  both
runtime and compile-time evaluation of expressions   that  are a mixture
between Prolog predicates  used  as   functions  and  built-in evaluable
terms.
*/
```
This is accomplished using goal expansion that moves any user defined functions into separate calls and connects the results back into the original expression via shared variables. This expansion is done when the module containing the expression is loaded, so dynamic evaluation of expressions with user defined functions was lost.

Another restriction of the existing functional arithmetic is that user defined functions are generally restricted to numeric arguments and return values. In many situations, functional notation is more expressive and improves readability compared to a conjunction of goals, which is largely why Prolog has used it for arithmetic for some time. But it would be useful if the same capability was extended to other types to support the creation of mini functional DSL's. Combining this with arithmetic evaluation supports mixed type expressions which include numbers. A few examples:
```
?- Ch = "D", Lower is "abcdefghijklmnopqrstuvwxyz"[Ch-"A"].  % convert `Ch` to lower case
Ch = "D",
Lower = d.

?- between(0,1,Den), between(-1,1,Num),
   Q is [[-inf,nan,inf][sign(Num)+1], Num/Den][Den\==0].     % divide by 0 check --> IEEE values
Den = 0,
Num = -1,
Q = -1.0Inf ;
Den = Num, Num = 0,
Q = 1.5NaN ;
Den = 0,
Num = 1,
Q = 1.0Inf ;
Den = 1,
Num = Q, Q = -1 ;
Den = 1,
Num = Q, Q = 0 ;
Den = Num, Num = Q, Q = 1.

?- A2 is ndarray([[4,7],[2,6]]), I2 is inverse(A2).          % flag(prefer_rationals)=true
A2 = #(#(4, 7), #(2, 6)),
I2 = #(#(3r5, -7r10), #(-1r5, 2r5)).
```
The `arithmetic_types` module included in this pack is an extension of `library(arithmetic)`. It enables list and atomic literals, as well as user defined types (compound terms),  as function arguments and return values. 

Extending the set of types available for use in arithmetic expressions raises the possibility of supporting polymorphic functions. These are functions with a common functor symbol and arity but whose semantics depends on the actual input arguments. For example, the same function template used for indexing or concatenating strings could be used for performing the same "function" for any sequence type, e.g., lists or arrays. (This is a familiar concept in object oriented languages where the same method signature is used in many classes; each class provides its own implementation (or inherits one from its superclass).)

There are a couple of options for implementing polymorphic functions in Prolog. Perhaps an obvious one is to view them as multiple clauses in a single predicate. If such a predicate was `multifile`, then each "type module" (a module implementing a set of functions for a user defined type), could implement a clause implementing the semantics for that type. This creates a dependency between the types, e.g., an inadvertent cut in one implementation could remove the option of success in another. And all implementations would have to agree on a module context "owning" the function in question.

An alternative model, and the one adapted in this design, is to treat each implementation as private, i.e., there need not be a user "callable" predicate interface to a function. Rather, each type module "registers" its implementation of a (polymorphic) function using the `arithmetic_function/1` directive. This approach enables each type module to be totally independent from a perspective of defining functions; no multifile requirement or common context to define. Once a function is registered, it is globally available for use in arithmetic evaluation. At the same time there is nothing restricting a user from defining a common callable predicate as described in the previous paragraph. Indeed it's fairly simple to define a function using a pre-existing predicate, e.g., `string_length/2` that fits the requirement of a "function predicate" (last argument is the result).

This second model is loosely analogous to how OOP languages provide polymorphism, i.e., each `class.method`  is an independent implementation (ignoring inheritance); it's just that the method selected is determined by the class, i.e., type, of the object involved. Prolog has no such concept, so the argument type checking must be performed by the function. If the checking fails, the function must fail to permit an alternative implementation a chance at success. The flip side is that functions are deterministic; once they succeed, the possibility of trying other function implementations via backtracking is eliminated.

From a scoping perspective, the second model is also similar to builtin arithmetic functions, i.e., they are globally available and not directly callable, e.g., there is no exported `max/3` predicate.

Module `arithmetic_types` supports the same `exports` list and core semantics as `arithmetic` (it was derived from a clone), but it performs term and goal expansion before `arithmetic` (context `user` before `system`). Effectively, it overrides `library(arithmetic)` once it is loaded.

Also included in this pack are a few "type modules" described below. These are meant to act as motivating examples; a starter kit rather than a definitive library. It includes examples of external predicates (i.e., imported from some module) (e.g., `lists:flatten`) and even system predicates (e.g., `length`) overloaded to implement as arithmetic functions. In these situations, care must be taken to define the intended local predicate as the function.

#### `type_bool`
As described above, it is recommended  to package groups of functions according to their type. So, for example, a `type_bool` module would define some functions with boolean argument values, e.g., `and`, `or`, and `not`, along with functions that produce boolean values, e.g., comparisons. The interface to such a module might look like:
```
:- module(type_bool,
	[
	 bool/1,
	 op(200, fy,  not),     % boolean 'not'
	 op(500, yfx, and),     % boolean 'and'
	 op(500, yfx, or)       % boolean 'or'
	]).

:- current_module(arithmetic_types) -> true ; use_module(library(arithmetic_types)).

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
Note that the exports list is quite small, just a test predicate for `bool` values and a few operators for convenience. The real "exports" is the list of global functions that can now be used in arithmetic expressions. Here's a subset of the implementation of the interface:
```
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
```
The atomic comparison functions compare atomic values, i.e., atoms, strings, numbers, etc. If the two arguments are numbers they are compared by value (unlike term comparison on numbers). For details see the source code (<prolog/type_bool.pl>).

Now there's nothing very new here, in fact this could have been done with `library(arithmetic)` if comparisons were limited to numbers. Moving on, let's see what can be done with Prolog lists. 

#### `type_list`
Lists are used to represent arbitrarily long sequences of terms. So  things that one might want to do with lists is index and slice them (like Python), return the length of a list, or generate a new list, e.g., from a range of numbers. In addition, we'll assume that a list evaluates to itself (identity function) if it's used as a function argument.

The interface for a `type_list` module might look like:
```
:- module(type_list,
	[
	op(100, yf,  []),    % support block notation
	op(500, yfx, \\),    % for appending (pseudo SQL ||)
	slice_parameters/4,  % slice support for blocks
	index_parameters/3   % index support for blocks
	]).
	
:- current_module(arithmetic_types) -> true ; use_module(library(arithmetic_types)).

:- arithmetic_function(new/2).        % create
:- arithmetic_function('[|]'/2).      % identity function for list items
:- arithmetic_function([]/1).         % block index
:- arithmetic_function([]/2).
:- arithmetic_function(: /2).         % slice (used with block indexing)
:- arithmetic_function(length/1).     % size or length
:- arithmetic_function(init/2).       % fill any vars
:- arithmetic_function(\\ /2).        % list concat
:- arithmetic_function(flatten/1).    % flattened list
:- arithmetic_function(arange/2).     % list from range(0,N)
:- arithmetic_function(arange/3).     % list from range(B,E)
:- arithmetic_function(arange/4).     % list from range(B,E,Step)
```
There already is a filter for type testing (`is_list`), so we don't need one of those; just a couple of operators for block notation and append (`\\`), and some generic support for indexing and slicing that could be used by other sequence types, e.g., strings. A subset of the defined functions:
```
%
% Function: generic slice expression - pass through until used
%
':'(B,E,B:E). 

%
% Function: evaluate list items
% 
'[|]'(X,Xs,[X|Xs]).        % lazy evaluation

%
% Function: create new list
%
new(list,Size,L) :- integer(Size), Size >= 0, (nonvar(L) -> is_list(L) ; true), !,
	length(L,Size).

new(list,Xs,Xs) :- is_list(Xs).

%
% Function: indexing and slicing
%
[](L, L) :- is_list(L).
[]([I1,I2|IN],T,X) :-  !,      % multi-level index, works on any supported indexing type
	T1 is T[I1],               % index one level and recurse
	X is T1[I2|IN].
[]([B:E],L,X) :- is_list(L),  
	length(L,Len),
	slice_parameters(B:E,Len,SB,SL), !,
	sub_list(L,SB,SL,_,X).
[]([Ix], L, R) :- is_list(L), 
	length(L,Len),
	index_parameters(Ix,Len,I),
	% the following uses near constant time arg for lists exceeding some threshold
	(I =< 28 -> skip_N(I,L,[X|_]) ; (T=..[$|L], arg(I,T,X))),
	item_eval(X,R).  % evaluate selected item
```
A few indexing and slicing examples:
```
?- T is [a,b,c][0].
T = a.

?- T is [a,b,c][-1].
T = c.

?- L is [a,b,c][1:2].
L = [b].

?- L is [a,b,c][0:2].
L = [a, b].

?- L is [a,b,c][1:E].
L = [b, c],
E = 3.

?- L is [a,b,c][1:1].
L =  ([]).

?- L is [a,b,c][1:0].
false.

?- N is [a,2+3,c][1].
N = 5.
```
Things to note:
- indicies are integers starting with `0` (first element); negative indicies are relative to the end of the list (like Python)
- slices are defined by a `B:E` term where `B` and `E` are indices. If `B` is a variable it is unified with 0; variable `E` is unified with the length of the list. 
- the result of indexing a list is a term which is then evaluated; if that fails, the result is the term itself. 
- the result of slicing a list is always another list.

Using `list` and `bool` functions, conditional expressions can be constructed as in the earlier examples (zero divide check). Or piecewise functions, e,g, the ramp function:
```
?- X = -1, H is [0,X][(X>=0)].
X = -1,
H = 0.

?- X =  2, H is [0,X][(X>=0)].
X = H, H = 2.
```
An expression converting upper case characters to lower case:
```
?- C='D', LowC is [C,"abcdefghijklmnopqrstuvwxyz"[[C]-['A']]][between('A','Z',C)].
C = 'D',
LowC = d.

?- C='3', LowC is [C,"abcdefghijklmnopqrstuvwxyz"[[C]-['A']]][between('A','Z',C)].
C = LowC, LowC = '3'.
```
Index notation can also be used on the right hand side of `is/2`:
```
?- length(L,2), L[0] is [2+3][0], L[1] is L[0]+1.
L = [5, 6].

?- length(L,2), L[_:_] is [a,b,c,d][1:3].
L = [b, c].
```
This works when the result of evaluating the right hand side of `is/2` unifies with the result of evaluating the expression. In these examples `L` is a list of variables so the unification succeeds. However, the arithmetic comparison predicates only support numeric values so, in general, these don't work with other types (either fail or generate errors). The atomic comparison functions supported in `type_bool` can sometimes be used as a workaround:
```
?- 1 is (2<3).
true.

?- 1 is (a>'A').
true.

?- 0 is ("abc">"abd").
true.
```
Module `type_list` supports a few other functions; see the source for details. Many of predicates in `library(lists)`, e.g., `reverse/2`, `max_member/2`, `min_member/2`, `sum_list/2`, etc., could  be easily added as functions.

#### `type_stringy`
SWI-Prolog `string`'s are another example of a sequence type, so it seems advantageous to use the same index and slice functions on strings. Furthermore, `string`'s and `atom`'s can almost be treated as equivalent from the perspective of functions applied to them. So here's the interface supported by `type_stringy`:
```
:- module(type_stringy,[stringy/1]).

:- current_module(arithmetic_types) -> true ; use_module(library(arithmetic_types)).
% uses operators and slice/indexing support from type_list
% for indexing and slicing support
:- current_module(type_list) 
	-> true 
	;  reexport(library(type_list)).      % reexport for operators

:- arithmetic_function(string/1).         % term to string conversion
:- arithmetic_function([]/1).             % indexing (char) and slicing (substring)
:- arithmetic_function([]/2).
:- arithmetic_function(\\ /2).            % concat
:- arithmetic_function(string_length/1).  % "builtin", no code required
:- arithmetic_function(find/2).           % find a substring (like Python)
```
Note that the list of functions include the same index, slice, and concatenation function definitions that `type_list` defined. But the function implementations will be different since they're operating on different types. Since the implementations are private to the module this won't be a problem, but the they must ensure that they only succeed for arguments of their type and fail otherwise, so other implementations can be attempted. That's why `type_list` specifically tested that the indexed argument was a list.

The implementation of `type_stringy`:
```
%
% most string functions work equally well on atoms
%
stringy(T) :- (string(T) ; atom(T)), !.

%
% Function: string conversion
%
string(Term,S)   :- 
	(string(Term) -> S=Term ; term_string(Term,S)).  % string function is idempotent

%
% Function: string indexing and slicing
%
[](St, St) :- stringy(St).
[]([B:E],St,X) :- stringy(St),
	string_length(St,Len),  % St is a string
	slice_parameters(B:E,Len,SB,SL), !,
	sub_string(St, SB, SL, _, X).            % slicing evaluates to a string
[]([Ix],St,X) :- stringy(St),
	string_length(St,Len),
	index_parameters(Ix,Len,I),
	sub_string(St, I, 1, _, SS), !,
	string_chars(SS,[X]).                    % indexing evaluates to a char

%
% Function: string concat (\\)
%
\\(S1,S2,R) :- stringy(S1), stringy(S2),     % arguments must be stringy
	string_concat(S1,S2,R), !.	             % deterministic concat

%
% Function: find - returns position of substring in string (like Python)
%
find(Sub,S,R) :- stringy(Sub), stringy(S),   % arguments must be stringy
	(sub_string(S,R,_,_,Sub)
	 -> true
	 ; R= -1  % not found value
	).	            
```
Note that exported predicates `index_parameters/3` and `slice_parameters/4` from `type_list` are used, so slicing and indexing semantics will be equivalent. Similarly, both `type_list` and `type_stringy` will define a module specific `\\ /3` predciate for concatenation. 

#### `type_ndarray`
Polymorphic functions, those which have different implementations depending on argument type, can be very useful. Another example of a sequence type is `ndarray`, also included in this pack, which provides a subset of Python's `ndarray` class (see [**NumPy**](https://numpy.org/doc/stable/reference/arrays.ndarray.html)). See the source for details, but a few examples:
```
?- A is ndarray([[1,2,3],[4,5,6]]).
A = #(#(1, 2, 3), #(4, 5, 6)).

?- A is new(ndarray,[3,3]), A[0,1] is 42, X is A[0:1][0][0:3-1][1].
A = #(#(_, 42, _), #(_, _, _), #(_, _, _)),
X = 42.

?- T is transpose(ndarray([[[1, 2], [5, 6]], [[3, 4], [7, 8]]])).
T = #(#(#(1, 3), #(5, 7)), #(#(2, 4), #(6, 8))).

?- S is sum(ndarray([1,2,3])).
S = 6.

?- CrossProd is cross(ndarray([1,2]),ndarray([4,5])).
CrossProd = -3.

% Product of a 3x3 matrix and its inverse is the 3x3 identity matrix
?- A is ndarray([[3,0,2],[2,0,-2],[0,1,1]]), I is inverse(A), X is dot(A,I).
A = #(#(3, 0, 2), #(2, 0, -2), #(0, 1, 1)),
I = #(#(1r5, 1r5, 0), #(-1r5, 3r10, 1), #(1r5, -3r10, 0)),
X = #(#(1, 0, 0), #(0, 1, 0), #(0, 0, 1)).
```
Note: To adhere to **NumPy** semantics, the implmentation of the `transpose` function was changed in version 0.1.0 version of this pack.

A somewhat more compelling example is to use `ndarray` to solve a system of linear equations.If the equation system is expressed in the matrix form **`A`**`• x = b`, the entire solution set can also be expressed in matrix form. If the matrix **`A`** is square (has m rows and n=m columns) and has full rank (all m rows are independent), then the system has a unique solution given by `x = `**`A`**<sup>`-1`</sup>`•b`.

For the set of equations:
```
x + y + z + w = 13 
2x + 3y − w = −1 
−3x + 4y + z + 2w = 10 
x + 2y − z + w = 1
```
Using flag `prefer_rationals=true`:
```
?- A is ndarray([[1,1,1,1],[2,3,0,-1],[-3,4,1,2],[1,2,-1,1]]), 
B is ndarray([[13],[-1],[10],[1]]), 
Vs is ndarray([[X],[Y],[Z],[W]]), 
Vs is dot(inverse(A),B).
A = #(#(1, 1, 1, 1), #(2, 3, 0, -1), #(-3, 4, 1, 2), #(1, 2, -1, 1)),
B = #(#(13), #(-1), #(10), #(1)),
Vs = #(#(2), #(0), #(6), #(5)),
X = 2,
Y = 0,
Z = 6,
W = 5.
```

#### Caveats
Over a number of major releases, SWI-Prolog arithmetic has acquired a few "idiosyncrasies"; legal literal terms which are not numbers syntactically. These include atoms representing 0 arity functions (e.g.,`e, pi, inf`) , lists of length 1 (e.g., `[a], [97]`), strings of length 1 (e.g., "a"), and dictionary support (e.g., `dict.key`). To maintain strict backwards compatibility, these continue to have the builtin arithmetic semantics:
```
?- X is [a,b,c,d,e][-1], Y is [a,b,c,d,e][0].
X = 2.718281828459045,
Y = a.

?- X is [a], Y is [a,b].
X = 97,
Y = [a, b].

?- X is "a", Y is "ab".
X = 97,
Y = "ab".
```
A flag to control the tradeoff between consistency and backwards compatibility is under consideration.

As with `library(arithmetic)`, goal expansion is done at load time and does not support dynamically created functions, e.g., `X0 = [1,2,3][0], Y = is X0+1.` In such cases, arithmetic function `eval` can be used:
```
?- X0 = [1,2,3][0], Y is X0+1.
ERROR: Type error: `[]' expected, found `[1,2,3]' (a list) ("x" must hold one character)
ERROR: In:
ERROR:   [11] _1296 is [](...,...)+1

?- X0 = [1,2,3][0], Y is eval(X0+1).
X0 = [1, 2, 3][0],
Y = 2.
```

#### `arithmetic` or `arithmetic_types`
Module `arithmetic_types` provides the functionality of `library(arithmetic)` but it must be explicitly loaded. If the latter has already been loaded (or autoloaded), loading `arithmetic_types` will result in an error message:
```
ERROR: import/1: No permission to import arithmetic_types:arithmetic_expression_value/2 into user (already imported from arithmetic)
```
This generally doesn't cause any problems because any failure by `arithmetic_types` to either expand math goals or evaluate expressions will fail to `library(arithmetic)`. In some rare cases calls to `arithmetic_expression_value` may require module qualification. 

#### Getting Started

Module `arithmetic_types` and the type modules described above are available as a downloadable pack:
```
?- pack_install(arithmetic_types, [url('https://github.com/ridgeworks/arithmetic_types.git')]).
	
?- use_module(library(arithmetic_types)).
true.
```
Once the pack has been installed, any of the provided type modules can be loaded using `use_module`.

As a general principle, generating errors is avoided but diagnostic messages indicating possible problems are output when `debug(arithmetic_types)` is enabled:
```
?- N=1, D=0, Q is [inf, N/D][D=\=0].
false.

﻿?- debug(arithmetic_types).
 true.
 
?- N=1, D=0, Q is [inf, N/D][D=\=0].
 % arithmetic_types: failed to evaluate 0=\=0 .
 % arithmetic_types: failed to evaluate [inf,1/0][0=\=0] .
 false.
```

