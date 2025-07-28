:- module(test_list_fns,
	  [ test_list_fns/0
	  ]).
:- use_module(library(plunit)).

:- if(true).  %:- if(exists_source(library(list_fns))).
:- use_module(library(type_list)).
%%:- current_module(type_list) -> true ; use_module(library(type_list)).

test_list_fns :-
	run_tests([ list_functions
	          ]).

:- begin_tests(list_functions).

test(sub_list) :-
	type_list:sub_list([1,2,3], 0,1, 2,[1]),
	type_list:sub_list([1,2,3], 1,2, 0,[2,3]),
	\+ type_list:sub_list([1,2,3], _,2, 0,[2,3]),
	\+ type_list:sub_list([1,2,3], 0,_, 0,[2,3]).

test(newlist) :-
	L is new(list,3),
	L = [_,_,_].

test(newlist, L == [1,2,3]) :-
	L is new(list,[1,2,3]).

test(indexlist) :-
	L is new(list,[1,2,3]),
	1 is L[0],
	3 is L[2],
	2 is L[2-1],
	3 is L[-1].

test(slicelist) :-             % also tests sub_list/5
	L is new(list,[1,2,3]),
	S1 is L[_:1], S1 = [1],
	S2 is L[1:3], S2 = [2,3],
	S2 is L[0-2:_].

test(lenlist) :- 
	3 is length(new(list,3)).

test(initlist) :- 
	L0 is init(new(list,3),0), L0 is [0,0,0],
	L1 is init(new(list,[_,1,_]),0), L1 is [0,1,0].

test(appendlist, L==[1,2,3,4]) :- 
	L is [1,2] \\ [3,4].

test(tolist, L==[1,1]) :-
	L is new(list,[1,1]).

test(tolist, L == [0,0]) :-
	L is new(list,2),
	L[0] is 0,
	L[1] is L[0].

test(tolist, L == [3,3,3]) :-
	L is new(list,3),
	L[0] is 3,
	L[1:3] is L[0:2].

test(flattenlist, T==[1,2,3,4]) :- 
	T is flatten([[1,2],[3,4]]).

test(arangelist, Li == [10, 12, 14, 16, 18, 20]) :-
	L is arange(list,3),
	L is arange(list,0,2),
	Li is arange(list,10,20,2).

:- end_tests(list_functions).

:- else.

test_list_fns.

:- endif.