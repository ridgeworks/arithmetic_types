:- module(type_list,
	[
	op(100, yf,  []),    % support block notation
	op(500, yfx, \\),    % for appending (pseudo SQL ||)
	slice_parameters/4,  % slice support for blocks
	index_parameters/3   % index support for blocks
	]).

/** <module> arithmetic type support for lists 

This module implements a set of functions on lists which can be used in standard arithmetic expressions, including block indexing and slicing (using `[]` as a postfix operator), concatenation, flatten, fill from a range, etc. It also exports a couple of predicates to support indexing and slicing on other "sequence" types.

The set of list arithmetic functions defined by this module include:
```
:- arithmetic_function(new/2).        % create
:- arithmetic_function('[|]'/2).      % evaluate list items
:- arithmetic_function([]/1).         % block index
:- arithmetic_function([]/2).
:- arithmetic_function(: /2).         % slice (used with block indexing)
:- arithmetic_function(length/1).     % size or length
:- arithmetic_function(init/2).       % fill any vars
:- arithmetic_function(\\ /2).        % list concat
:- arithmetic_function(flatten/1).    % flattened list
:- arithmetic_function(arange/2).     % list from range(N)
:- arithmetic_function(arange/3).     % list from range(B,E)
:- arithmetic_function(arange/4).     % list from range(B,E,S)
```

See the ReadMe for this pack for more documentation and examples.
*/

:- use_module(library(arithmetic_types)).
%%:- current_module(arithmetic_types) -> true ; use_module(library(arithmetic_types)).

% Also provides:
%  1. Generic slice evaluation (used inside block indexing)
%  2. Generic evaluation of list items

:- set_prolog_flag(unknown,fail).     % disable autoloading of other function predicates

:- arithmetic_function(new/2).        % create
:- arithmetic_function('[|]'/2).      % evaluate list items
:- arithmetic_function([]/1).         % block index
:- arithmetic_function([]/2).
:- arithmetic_function(: /2).         % slice (used with block indexing)
%- arithmetic_function(length/1).     % size or length (directive below)
:- arithmetic_function(init/2).       % fill any vars
:- arithmetic_function(\\ /2).        % list concat
:- arithmetic_function(flatten/1).    % flattened list (directive below)
:- arithmetic_function(arange/2).     % list from range(N)
:- arithmetic_function(arange/3).     % list from range(B,E)
:- arithmetic_function(arange/4).     % list from range(B,E,S)

%
% Exports
%
% slice parms
slice_parameters(B:E,Len,SBegin,SLen) :-
	item_eval(B,Br), (var(Br) -> Br=0 ; integer(Br)),
	item_eval(E,Er), (var(Er) -> Er=Len ; integer(Er)),
	(Br<0 -> SBegin is Len+Br ; SBegin=Br),
	(Er<0 -> SLen is Len+Er-SBegin ; SLen is Er-SBegin).

% index parm
index_parameters(Ix,Len,I) :-
	item_eval(Ix,EIx),
	integer(EIx),
	(EIx < 0 -> I is Len+EIx ; I = EIx),
	I >= 0.

% evaluate (largely for efficiency)
item_eval(X,X) :- var(X), !.                          % vars OK in lists
item_eval(N,N) :- number(N), !.                       % optimization
item_eval(X,R) :- 
	catch(arithmetic_expression_value(X,R), _, R=X).  % catchall, identity function

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
    
%  sub_atom/5 for lists
sub_list(L,Before,Length,After,SubL) :- integer(Before), integer(Length), is_list(L),
	skip_N(Before,L,L1),         % remove prefix
	next_N(Length,L1,SubL,L2),   % collect sub list and suffix	
	length(L2,After),            % length of suffix
	!.                           % deterministic

skip_N(0,In,In):- !.
skip_N(1,[_|In],In):- !.
skip_N(N,[_,_|Xs],Out) :-                  % N>0,  % superfluous check
	N1 is N-2,
	skip_N(N1,Xs,Out).

next_N(0,In,[],In) :- !.
next_N(1,[X|In],[X],In) :- !.
next_N(N,[X1,X2|In],[X1,X2|Out],Rem) :-    % N>0,  % superfluous check
	N1 is N-2,
	next_N(N1,In,Out,Rem).

%
% Function: size/length (never called locally, prior calls invoke system:length)
%
:- redefine_system_predicate(length(_,_)). % permits local redefinition for function

length(L,N) :- is_list(L),
	system:length(L,N).

:- arithmetic_function(length/1).          % size or length

%
% Function: fill any vars
%
init(L, Value, L) :- is_list(L),
	fill_each(L,Value).

fill_each([],_).
fill_each([X|Xs],Value) :-
	(is_list(X)
	 -> fill_each(X,Value)
	  ; (var(X) -> X=Value ; true)
	),
	fill_each(Xs,Value).

%
% Function: append 2 lists
%
\\(L1, L2, R) :-  nonvar(L1), is_list(L2),  % guard against ill-formed lists
	append_det(L1,L2,R).

append_det([], L, L) :- !.  % deterministic, so !
append_det([H|T], L, [H|R]) :-
	append_det(T, L, R).

%
% Function: flattened list
%
flatten(List,FList) :- is_list(List),
	lists:flatten(List,FList).

:- arithmetic_function(flatten/1).    % flattened list from local flatten/2

%
% Function: arange/2,3,4
%
arange(list,N,L) :- number(N), N>0,
	E is N-1,
	arange_(0,E,1,L).

arange(list,B,E,L) :- number(B), number(E),
	B>=0, E>B,
	arange_(B,E,1,L).

arange(list,B,E,S,L) :- number(B), number(E), number(S),
	B>=0, E>B, S>0,
	arange_(B,E,S,L).

arange_(B,E,_S,[]) :- B>E, !.
arange_(B,E,S,[B|Vs]) :- 
	B1 is B+S,
	arange_(B1,E,S,Vs).
