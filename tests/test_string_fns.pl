:- module(test_string_fns,
	  [ test_string_fns/0
	  ]).
:- use_module(library(plunit)).

:- if(true).  %:- if(exists_source(library(type_string))). 
:- use_module(library(type_stringy)).
%%:- (current_module(type_stringy) -> true ; use_module(library(type_stringy))).

test_string_fns :-
	run_tests([ string_functions
	          ]).

:- begin_tests(string_functions).

test(f_string) :-
	"abc" is string(abc), %S1 == "abc",
	"42" is string(42), %S2 == "42",
	S3 is string(2+2), S3 == "4",
	S3 is string(string(2+2)),
	S4 is string([1,2]), S4 == "[1,2]",
	S5 is string(_), sub_string(S5,0,1,_,"_").  % some var

test(indexstring) :-  % indexing evaluates to character
	'1' is "123"[0],
	S3 is "123"[2], S3 == '3',
	'2' is "123"[2-1],
	S3 is "123"[-1],
	\+ _ is "123"[3],
	\+ _ is "123"[-4].

test(slicestring, S2=="23") :-             % also tests sub_list/5
	"1" is "123"[_:1], %S1 == "1",         % because '1' is "1" evaluates to char code
	S2 is "123"[1:3], %S2 == "23",
	S2 is "123"[0-2:_].

test(stringconcat, S1=="abcd") :-
	S1 is "ab" \\ "cd",
	S1 is string(ab) \\ string(cd),
	"ab12" is string(ab) \\ string(12).

test(stringlength) :-
	0 is string_length(""),
	5 is string_length("12345").
	 
test(stringfind) :-
	0 is find(a,abc),
	1 is find("bc","abc"),
	-1 is find("e",abc).

:- end_tests(string_functions).

:- else.

test_string_fns.

:- endif.