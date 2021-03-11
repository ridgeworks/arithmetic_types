/*  Cloned from arithmetic.pl, Part of SWI-Prolog
	Modified for user defined arithmetic types by Rick Workman, 2021

	Author:        Jan Wielemaker
	E-mail:        J.Wielemaker@vu.nl
	WWW:           http://www.swi-prolog.org
	Copyright (c)  2011-2015, VU University Amsterdam
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions
	are met:

	1. Redistributions of source code must retain the above copyright
	   notice, this list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above copyright
	   notice, this list of conditions and the following disclaimer in
	   the documentation and/or other materials provided with the
	   distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
	"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
	LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
	FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
	COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
	INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
	BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
	LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
	ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
	POSSIBILITY OF SUCH DAMAGE.
*/

:- module(arithmetic_types,
		  [ arithmetic_function/1,              % +Name/Arity
			arithmetic_expression_value/2       % Expression, -Value
		  ]).
:- autoload(library(error),[type_error/2]).     % for compile/load errors

%:- set_prolog_flag(generate_debug_info, false).

/** <module> Extensible arithmetic types

This module extends the existing library(arithmetic) to support expressions
with atomic values and user defined types. Such types include compound 
terms which are not functions. A user defined type is typically packaged 
as a module with its associated function definitions. These predicates need
not, and typically do not export the predicates; the exports of such modules
are the set of globally visible functions they define. Functions can be
"polymorphic" in that the same function Name/Arity can be defined for more
than one type; in such cases they are distinguished by the argument types.

This module extends the functionality of library(arithmetic) and exports
the same predicate set. Conficts are largely avoided since arithmetic type
expansion is done  before library(arithmetic) expansion is invoked.

Functions are defined using the directive  arithmetic_function/1. Runtime
evaluation  is provided by arithmetic_expression_value/2.
*/

:- multifile evaluable/2.                       % Term, Module

%!  arithmetic_function(NameArity) is det.
%
%   Declare a predicate as an arithmetic function.
%
arithmetic_function(Term) :-
	throw(error(context_error(nodirective, arithmetic_function(Term)), _)).

arith_decl_clauses(NameArity, Clauses) :-
	pred_indicator(NameArity,Name,Arity)
	 -> 
		compound_name_arity(Term, Name, Arity),  %  for possible 0 arity
		ImplArity is Arity+1,
		functor(Pred, Name, ImplArity),
		prolog_load_context(module, M),
		defining_context(M:Pred,Q),
		(evaluable(Term, Q)                      % make idempotent
		 -> Clauses=[]
		 ;  Clauses=[arithmetic_types:evaluable(Term, Q)]
		)
	 ;  type_error(predicate_indicator, NameArity).

pred_indicator(_:NameArity, Name, Arity) :- % for compatibility - throw away any specified module
	pred_indicator(NameArity, Name, Arity).
pred_indicator(Name/Arity, Name, Arity). 
  
defining_context(Pred,M) :- 
	predicate_property(Pred,implementation_module(M)), !.  % local to M  
defining_context(Pred,C) :- 
	predicate_property(Pred,imported_from(C)), !.          % imported from C          
defining_context(_,user).                                  % not found, sorted out at evaluation? 

%!  eval_clause(+Term, -Clause) is det.
%
%   Clause is a clause for evaluating  the  built-in arithmetic expression
%   Term.

eval_clause(roundtoward(_,Round), (eval(Gen,Result) :- Body)) :-
	!,
	Gen = roundtoward(Arg,Round),
	eval_args([Arg], [PlainArg], Goals,
			  [Result is roundtoward(PlainArg,Round)]),
	list_conj(Goals, Body).
eval_clause(Term, (eval(Gen, Result) :- Body)) :-
	functor(Term, Name, Arity),
	functor(Gen, Name, Arity),
	Gen =.. [_|Args],
	eval_args(Args, PlainArgs, Goals, [Result is NewTerm]),
	NewTerm =.. [Name|PlainArgs],
	list_conj(Goals, Body).

eval_args([], [], Goals, Goals).
eval_args([E0|T0], [A0|T], [eval(E0, A0)|GT], RT) :-
	eval_args(T0, T, GT, RT).

list_conj([], !).
list_conj([H|T0], (H,T)) :-
	list_conj(T0, T).

eval_clause(Clause) :-
	current_arithmetic_function(Term),
	eval_clause(Term, Clause).

term_expansion(eval('$builtin', _), Clauses) :-
	findall(Clause, eval_clause(Clause), Clauses).

%
% Compile arithmetic in eval/2 (particularly '$builtin')
%
:-  (current_prolog_flag(optimise, Opt),
	 nb_setval('arithmetic_types:optflag',Opt),  % save current value to restore later
	 set_prolog_flag(optimise,true)
	).

restore_optimise :-  % restore "optimise" flag
	(nb_current('arithmetic_types:optflag', Opt) 
	 -> (nb_delete('arithmetic_types: optflag'), set_prolog_flag(optimise,Opt))
	 ; true
	).

%!  arithmetic_expression_value(:Expression, -Result) is det.
%
%   True  when  Result  unifies  with    the  arithmetic  result  of
%   evaluating Expression.

arithmetic_expression_value(Expression, Result) :-
	eval(Expression, Result).

eval(Var, _) :-            % var check to prevent infinite eval loop
	var(Var),
	!, 
	eval_error(Var).
eval(Number, Number) :-    % first numbers
	number(Number),
	!.
eval(Term, Result) :-      % then user defined functions
	eval_user(Term, Result),
	!.
eval('$builtin', _).       % then builtins (expanded at load time)
eval(Literal, Result) :- atom(Literal),  % then if atom, maybe 0 arity user function
	compound_name_arity(Term,Literal,0),
	eval_user(Term, Result),
	!.
eval(Literal, Literal) :- atomic(Literal),  % then other literals - evaluate to themselves
	!.
eval(Term, Result) :-      % then see if library(arithmetic) works
	current_module(arithmetic),
	catch(arithmetic:arithmetic_expression_value(Term, Result),_,fail),
	!.
eval(Term, _Result) :-     % then fail
	eval_error(Term).

:- restore_optimise.   % end of eval, restore optimise flag

%
% evaluate user defined function (via inline code or arithmetic_expression_value)
%
eval_user(Function, Result) :-
	evaluable(Function, Module),    % non-deterministic due to possible polymorphism
	call(Module:Function, Result),
	!.  % commit to successful choice
	
%
% evaluation error - if debug, print warning, always fail
%
eval_error(Term) :-
	current_prolog_flag(debug, true),  % if false, silent fail
	print_message(warning, arithmetic_types(Term)),
	fail.

prolog:message(arithmetic_types(Term)) -->
	['arithmetic_types: failed to evaluate ~w .'-[Term] ].

				 /*******************************
				 *         COMPILE-TIME         *
				 *******************************/

math_goal_expansion(A is Expr, Goal) :-
	expand_function(A, NativeA, PreA),  % new
	expand_function(Expr, Native, Pre),
	tidy((PreA, Pre, NativeA is Native), Goal).
math_goal_expansion(ExprA =:= ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA =:= NativeB), Goal).
math_goal_expansion(ExprA =\= ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA =\= NativeB), Goal).
math_goal_expansion(ExprA > ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA > NativeB), Goal).
math_goal_expansion(ExprA < ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA < NativeB), Goal).
math_goal_expansion(ExprA >= ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA >= NativeB), Goal).
math_goal_expansion(ExprA =< ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA =< NativeB), Goal).

expand_function(Expression, NativeExpression, Goal) :-
	do_expand_function(Expression, NativeExpression, Goal0),
	tidy(Goal0, Goal).

do_expand_function(X, X, true) :-              % #1 anything evaluable
	evaluable(X),
	!.
do_expand_function(roundtoward(Expr0, Round),  % #2 roundtoward special case
				   roundtoward(Expr, Round),
				   ArgCode) :-
	!,
	do_expand_function(Expr0, Expr, ArgCode).
do_expand_function(X, Result, Result=X) :-     % #3 lists, move out of expression
	is_list(X),
	!.
do_expand_function(Function,                   % #4 user defined (before built in for overloading)
				   Result,
				   (ArgCode, arithmetic_types:eval_user(Pred,Result))) :-  % Use eval/2 for polymorphic functions
	evaluable(Function, _Module),	
	!,
	compound_name_arguments(Function, Name, Args),
	expand_predicate_arguments(Args, PredArgs, ArgCode),
	Pred =.. [Name|PredArgs].
do_expand_function(Function,                   % #5  builtin (before atomic for family of pi)
				   Result,
				   ArgCode) :-
	callable(Function),  % guard before
	current_arithmetic_function(Function),
	!,
	Function =.. [Name|Args],
	expand_function_arguments(Args, ArgResults, ArgCode),
	Result =.. [Name|ArgResults].
do_expand_function(Name,                       % #6 atom, possible user defined arity 0
				   Result,
				   arithmetic_types:eval_user(Pred,Result)) :-
	atom(Name),
	compound_name_arguments(Pred, Name, []),
	evaluable(Pred, _Module),
	!.    
do_expand_function(X, Result, Result=X) :-     % #7 atomic literals, move out of expression
	atomic(X),
	!.
do_expand_function(_Function, _, _) :-         % #8 fail expansion (defaults to 'arithmetic')
	fail.  % type_error(evaluable, Function).


expand_function_arguments([], [], true).
expand_function_arguments([H0|T0], [H|T], (A,B)) :-
	do_expand_function(H0, H, A),
	expand_function_arguments(T0, T, B).

expand_predicate_arguments([], [], true).
expand_predicate_arguments([H0|T0], [H|T], (A,B)) :-
	do_expand_function(H0, H1, A0),
	(   callable(H1),
		current_arithmetic_function(H1)
	->  A = (A0, H is H1)
	 ;  (A0 = (X=R) -> X=R, A=true ; A = A0),  % optimization for atomics
		H = H1
	),
	expand_predicate_arguments(T0, T, B).

%!  evaluable(F) is semidet.
%
%   True if F and all its subterms are variables or evaluable terms by builtin functions.
%
evaluable(F) :-
	var(F),
	!.
evaluable(F) :-
	number(F),
	!.
evaluable([Code]) :- 
	% assumes possibility of future environment flag to disable
	(current_prolog_flag(disable_codeTBD,true) -> fail ; eval_code(Code)),
	!.
evaluable(Func) :-                   % Functional notation.
	functor(Func, ., 2),
	!.
evaluable(F) :-                      % unfortunate case - should be a literal
	string(F),
	!,
	string_length(F, 1).
evaluable(roundtoward(F,_Round)) :-  % special case to ignore atom(_Round)
	!,
	evaluable(F).
evaluable(F) :-
	current_arithmetic_function(F),
	\+ evaluable(F,_),               % ** not overridden **
	(   compound(F)
	->  forall(arg(_,F,A), evaluable(A))
	;   true
	).

% as defined by builtin
eval_code(Code) :- var(Code).
eval_code(Code) :- integer(Code), Code>=0.
eval_code(Code) :- atom(Code), atom_length(Code,1).

%!  tidy(+GoalIn, -GoalOut)
%
%   Cleanup the output from expand_function/3.

tidy(A, A) :-
	var(A),
	!.
tidy(((A,B),C), R) :-
	!,
	tidy((A,B,C), R).
tidy((true,A), R) :-
	!,
	tidy(A, R).
tidy((A,true), R) :-
	!,
	tidy(A, R).
tidy((A, X is Y), R) :-
	var(X), var(Y),
	!,
	tidy(A, R),
	X = Y.
tidy((A,B), (TA,TB)) :-
	!,
	tidy(A, TA),
	tidy(B, TB).
tidy(A, A).


				 /*******************************
				 *        EXPANSION HOOK        *
				 *******************************/

:- multifile                     % context = 'user' so run before 'arithmetic'
	user:term_expansion/2,
	user:goal_expansion/2.

user:term_expansion((:- arithmetic_function(Term)), Clauses) :-
	arith_decl_clauses(Term, Clauses).

user:goal_expansion(Math, MathGoal) :-  % failure will default to 'arithmetic'
	math_goal_expansion(Math, MathGoal).
