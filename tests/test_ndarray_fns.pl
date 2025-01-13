:- module(test_ndarray_fns,
	  [ test_ndarray_fns/0
	  ]).
:- use_module(library(plunit)).

:- if(true).  %:- if(exists_source(library(type_ndarray))).
:- use_module(library(type_ndarray)).
%%:- current_module(type_ndarray) -> true ; use_module(library(type_ndarray)).

test_ndarray_fns :-
	run_tests([ ndarray_functions
	          ]).

:- begin_tests(ndarray_functions).

test(newndarray) :-
	A is new(ndarray,[2,3,4]),
	ndarray(A),
	[2,3,4] is shape(A),
	3 is ndim(A),
	24 is size(A).

test(listndarray) :-
	L = [[1,2,3],[4,5,6]],
	A is ndarray(L),
	ndarray(A),
	[2,3] is shape(A),
	2 is ndim(A),
	6 is size(A),
	L is to_list(A).

test(indexndarray, X == 42) :-
	A is new(ndarray,[2,2]),
	A[0][1] is 42,
	X is A[0][1].
	
test(indexndarray, X == 42) :-
	A is new(ndarray,[2,2]),
	A[0,3-3] is 42,
	X is A[5*0,-2].
	
test(slicendarray, X == 42) :-  
	A is new(ndarray,[3,3]),
	A[0,1] is 42,
	X is A[0:1][0][0:3-1][1].
	
test(slicendarray) :-
	A is ndarray([1,2,3]),
	[1,2] is to_list(A[_: -1]),
	[2,3] is to_list(A[-2:_]).
	
test(concatndarray,L==[1,2,3,4]) :-
	L is to_list(ndarray([1,2])\\ndarray([3,4])).

test(concatndarray,L==[[1, 2], [3, 4]]) :-
	L is to_list(ndarray([[1,2]])\\ndarray([[3,4]])).

test(concatndarray,L==[[1, 2], [11, 12], [3, 4], [13, 14]]) :-
	L is to_list(ndarray([[1,2],[11,12]]) \\ ndarray([[3,4],[13,14]])).

test(flattenndarray, T==[1,2,3,4]) :- 
	T is to_list(flatten(ndarray([[1,2],[3,4]]))).

test(reshapendarray, T==[1,2,3,4,5,6]) :- 
	T is to_list(reshape(ndarray([[1,2,3], [4,5,6]]),[6])).

test(reshapendarray, T==[[0, 1, 2], [3, 4, 5]]) :- 
	T is to_list(reshape(arange(ndarray,6),[2,3])).

test(transposendarray, T==[1,2,3]) :- 
	T is to_list(transpose(ndarray([1,2,3]))).

test(transposendarray, T==[[1,2,3]]) :- 
	T is to_list(transpose(ndarray([[1],[2],[3]]))).
	
test(transposendarray, T==[[1,3],[2,4]]) :- 
	T is to_list(transpose(ndarray([[1,2],[3,4]]))).

test(transposendarray, T==[[[0, 4, 8],[2, 6, 10]],[[1, 5, 9],[3, 7, 11]]]) :- 
	T is to_list(transpose(ndarray([[[0,1],[2,3]],[[4,5],[6,7]],[[8,9],[10,11]]]))).

test(transposendarray, L==L0) :- 
	L0 = [1,2,3],
	T0 is transpose(ndarray(L0)),
	L0 is to_list(transpose(T0)),
	L is to_list(T0).
	
test(transposendarray, L==[[[1, 5], [3, 7]], [[2, 6], [4, 8]]]) :- 
	L0 is [[[1,2],[3,4]],[[5,6],[7,8]]],
	T0 is transpose(ndarray(L0)),
	L0 is to_list(transpose(T0)),
	L is to_list(T0).

test(initndarray, L==[42,42,42]) :- 
	A is init(new(ndarray,[2,3]),42),
	L is to_list(A[0]),
	L is to_list(A[1]).

test(arangendarray, X==[0]) :- 
	X is to_list(arange(ndarray,1)),
	[1,2,3] is to_list(arange(ndarray,1,4)),
	[10,12,14,16,18] is to_list(arange(ndarray,10,20,2)).

test(idndarray, X==[1]) :- 
	X is to_list(identity(ndarray,1)),
	[[1, 0], [0, 1]] is to_list(identity(ndarray,2)),
	\+ X is identity(ndarray,0).

test(sumndarray) :- 
	6 is sum(ndarray([1,2,3])),
	21 is sum(ndarray([[1,2,3],[4,5,6]])).
	
test(minndarray) :- 
	1 is min(ndarray([1,2,3])),
	1 is min(ndarray([[4,5,6],[1,2,3]])).
	
test(maxndarray) :- 
	3 is max(ndarray([1,2,3])),
	6 is max(ndarray([[1,2,3],[4,5,6]])).
	

test(addndarray) :-
	A1 is init(new(ndarray,[1]),2),
	A2 is init(new(ndarray,[1]),3),
	AR is A1 .+ A2,
	AR is A1 .+ 3,
	AR is 2 .+ A2,
	AR is init(new(ndarray,[1]),5).
		
test(addndarray) :-
	A1 is init(new(ndarray,[2,2,3]),2),
	A2 is init(new(ndarray,[2,2,3]),3),
	AR is A1 .+ A2,
	AR is A1 .+ 3,
	AR is 2 .+ A2,
	AR is init(new(ndarray,[2,2,3]),5).
	
test(minusndarray) :-
	A1 is init(new(ndarray,[1]),2),
	AR is .- A1 ,
	AR is init(new(ndarray,[1]),-2).
	
test(minusndarray) :-
	A1 is init(new(ndarray,[2,2,3]),2),
	AR is .- A1 ,
	AR is init(new(ndarray,[2,2,3]),-2).
	
test(subndarray) :-
	A1 is init(new(ndarray,[1]),2),
	A2 is init(new(ndarray,[1]),3),
	AR is A1 .- A2,
	AR is A1 .- 3,
	AR is 2 .- A2,
	AR is init(new(ndarray,[1]),-1).
	
test(subndarray) :-
	A1 is init(new(ndarray,[2,2,3]),2),
	A2 is init(new(ndarray,[2,2,3]),3),
	AR is A1 .- A2,
	AR is A1 .- 3,
	AR is 2 .- A2,
	AR is init(new(ndarray,[2,2,3]),-1).
	
test(mulndarray) :-
	A1 is init(new(ndarray,[1]),2),
	A2 is init(new(ndarray,[1]),3),
	AR is A1 .* A2,
	AR is A1 .* 3,
	AR is 2 .* A2,
	AR is init(new(ndarray,[1]),6).
	
test(mulndarray) :-
	A1 is init(new(ndarray,[2,2,3]),2),
	A2 is init(new(ndarray,[2,2,3]),3),
	AR is A1 .* A2,
	AR is A1 .* 3,
	AR is 2 .* A2,
	AR is init(new(ndarray,[2,2,3]),6).
	
test(divndarray) :-
	A1 is init(new(ndarray,[1]),4),
	A2 is init(new(ndarray,[1]),2),
	AR is A1 ./ A2,
	AR is A1 ./ 2,
	AR is 4 ./ A2,
	AR is init(new(ndarray,[1]),2).
	
test(divndarray) :-
	A1 is init(new(ndarray,[2,2,3]),4),
	A2 is init(new(ndarray,[2,2,3]),2),
	AR is A1 ./ A2,
	AR is A1 ./ 2,
	AR is 4 ./ A2,
	AR is init(new(ndarray,[2,2,3]),2).

test(powndarray) :-
	A1 is init(new(ndarray,[1]),4),
	A2 is init(new(ndarray,[1]),2),
	AR is A1 .** A2,
	AR is A1 .** 2,
	AR is 4 .** A2,
	AR is init(new(ndarray,[1]),16).
	
test(powndarray) :-
	A1 is init(new(ndarray,[2,2,3]),4),
	A2 is init(new(ndarray,[2,2,3]),2),
	AR is A1 .** A2,
	AR is A1 .** 2,
	AR is 4 .** A2,
	AR is init(new(ndarray,[2,2,3]),16).

test(applyndarray) :-
	A is init(new(ndarray,[1]),4),
	AR is apply(sqrt,A),
	AR is init(new(ndarray,[1]),2.0).
	
test(applyndarray) :-
	A is init(new(ndarray,[2,2,3]),0),
	AR is apply(exp,A),
	AR is init(new(ndarray,[2,2,3]),1.0).

test(crossndarray) :-
	-3 is cross(ndarray([1,2]),ndarray([4,5])),
	[-3,6,-3] is to_list(cross(ndarray([1,2,3]),ndarray([4,5,6]))).
	
test(innerndarray) :-
	6 is inner(ndarray([2]),ndarray([3])),
	2 is inner(ndarray([1,2,3]),ndarray([0,1,0])),
	\+ _ is inner(ndarray([1,2,3]),ndarray([0,1,0,1])),
	\+ _ is inner(ndarray([[1,2,3],[4,5,6]]),ndarray([1,2,3])).
	
test(outerndarray, X==[12]) :-
	X is to_list(outer(ndarray([4]),ndarray([3]))),
	[12,15] is to_list(outer(ndarray([4,5]),ndarray([3]))),
	[8,12] is to_list(outer(ndarray([4]),ndarray([2,3]))),
	A1 is outer(ndarray([4,5]),ndarray([1,2,3])),
	A1 is transpose(outer(ndarray([1,2,3]),ndarray([4,5]))),
	[[4, 8, 12],[5, 10, 15]] is to_list(A1).

test(dotndarray) :-
	-30 is dot(ndarray([-6]),ndarray([5])),
	122 is dot(ndarray([9,2,7]),ndarray([4,8,10])),
	[[37, 40], [85, 92]] is to_list(dot(ndarray([[1,2],[3,4]]),ndarray([[11,12],[13,14]]))),
	[[10, 6, 12], [9, 6, 12], [12, 8, 16]] is to_list(dot(ndarray([[2,2],[0,3],[0,4]]),ndarray([[2,1,2],[3,2,4]]))).

test(detndarray) :-
	6 is determinant(ndarray([6])),
	-14 is determinant(ndarray([[3,8],[4,6]])),
	-306 is determinant(ndarray([[6,1,1],[4,-2,5],[2,8,7]])),
	-26 is determinant(ndarray([[-1,1,4,2],[2,-1,2,5],[1,2,3,4],[3,4,-1,2]])),
	2480 is determinant(ndarray([[0,6,-2,-1,5],[0,0,0,-9,-7],[0,15,35,0,0],[0,-1,-11,-2,1],[-2,-2,3,0,-2]])).
	
test(invndarray, [condition(current_prolog_flag(prefer_rationals, false)),L==[0.125]]) :- 
	L is to_list(inverse(ndarray([8.0]))),
	A2 is ndarray([[3,2],[4,3]]), I2 is inverse(A2), identity(ndarray,2) is dot(A2,I2),
	A3 is ndarray([[2,0,0],[0,3,2],[0,4,3]]),I3 is inverse(A3),X3 is dot(A3,I3), X3 is identity(ndarray,3).*1.0.

test(invndarray, [condition(current_prolog_flag(prefer_rationals, true)), L==[1r8]]) :- 
	L is to_list(inverse(ndarray([8]))),
	A2 is ndarray([[4,7],[2,6]]), I2 is inverse(A2), identity(ndarray,2) is dot(A2,I2),
	A3 is ndarray([[3,0,2],[2,0,-2],[0,1,1]]), I3 is inverse(A3), X3 is dot(A3,I3), X3 is identity(ndarray,3),
	A3a is ndarray([[-2,-2,1],[-4,-8,4],[-1,5,0]]),I3a is inverse(A3a), identity(ndarray,3) is dot(A3a,I3a),
	A4 is ndarray([[5,6,6,8],[2,2,2,8],[6,6,2,8],[2,3,6,7]]),I4 is inverse(A4), X4 is dot(A4,I4), X4 is identity(ndarray,4).

test(lineqndarray) :-
	A is ndarray([[1,3,-2],[3,5,6],[2,4,3]]),
	Vs is ndarray([[X],[Y],[Z]]),
	B is ndarray([[5],[7],[8]]),
	Vs is dot(inverse(A),B),
	X =:= -15, Y =:= 8, Z =:= 2.


:- end_tests(ndarray_functions).

:- else.

test_ndarray_fns.

:- endif.