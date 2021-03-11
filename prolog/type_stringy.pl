:- module(type_stringy,[stringy/1]).

:- current_module(arithmetic_types) -> true ; use_module(library(arithmetic_types)).
% uses operators and slice/indexing support from type_list
:- (current_module(type_list) -> true ; use_module(library(type_list))).

:- arithmetic_function(string/1).         % term to string conversion
:- arithmetic_function([]/1).             % indexing (char) and slicing (substring)
:- arithmetic_function([]/2).
:- arithmetic_function(\\ /2).            % concat
:- arithmetic_function(string_length/1).  % "builtin", no code required
:- arithmetic_function(find/2).           % find a substring (like Python)

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
