:- module(type_stringy,[stringy/1]).

%:- reexport(library(type_list),[op(_,_,_)]).  % requires fix circa 9.3.19 % for operators
:- reexport(library(type_list),except([slice_parameters/4,index_parameters/3])).   % for operators

/** <module> arithmetic type support for strings and atoms 

This module implements a set of functions on string types including strings and atoms. The only exported predicate is the type checking `stringy/1` which is true is its argument is a string or atom. Block-like indexing and slicing which construct substrings, are equivalent to similar opertions on lists. 

The set of `stringy` arithmetic functions defined by this module include:
```
:- arithmetic_function(string/1).         % term to string conversion
:- arithmetic_function([]/1).             % indexing (char) and slicing (substring)
:- arithmetic_function([]/2).
:- arithmetic_function(\\ /2).            % concat
:- arithmetic_function(string_length/1).  % "builtin", no code required
:- arithmetic_function(find/2).           % find a substring (like Python)
```

See the ReadMe for this pack for more documentation and examples.
*/

:- use_module(library(arithmetic_types)).
% for indexing and slicing support
:- use_module(library(type_list)).

:- arithmetic_function(string/1).         % term to string conversion
:- arithmetic_function([]/1).             % indexing (char) and slicing (substring)
:- arithmetic_function([]/2).
:- arithmetic_function(\\ /2).            % concat
:- arithmetic_function(string_length/1).  % "builtin", no code required
:- arithmetic_function(find/2).           % find a substring (like Python)

/** 
stringy(?X:stringy) is semidet

Succeeds if X is of type `stringy`, i.e., a string or atom; otherwise fails.
*/
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
	string_concat(S1,S2,R), !.               % deterministic concat

%
% Function: find - returns position of substring in string (like Python)
%
find(Sub,S,R) :- stringy(Sub), stringy(S),   % arguments must be stringy
	(sub_string(S,R,_,_,Sub)
	 -> true
	 ; R= -1  % not found value
	).
