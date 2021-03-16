:- module(type_ndarray,
	[ ndarray/1,
	  op(500, yfx, '.+'),      % arithmetic operators on arrays
	  op(200, fy,  '.-'),      % same precedence and associativity as standard ops
	  op(500, yfx, '.-'),
	  op(400, yfx, '.*'),
	  op(400, yfx, './') , 
	  op(200, xfx, '.**')  
	]).

:- current_module(arithmetic_types) -> true ; use_module(library(arithmetic_types)).
% for indexing and slicing support, arange/n content
:- current_module(type_list) -> true ; use_module(library(type_list)).

:- arithmetic_function(new/2).        % new from shape, uninitialized
:- arithmetic_function(ndarray/1).    % new from ListOfLists
:- arithmetic_function(to_list/1).    % list from ndarray
:- arithmetic_function(ndim/1).       % number of dimensions
:- arithmetic_function(shape/1).      % shape (a list)
:- arithmetic_function(size/1).       % number of values
:- arithmetic_function([]/1).         % block indexing and slicing
:- arithmetic_function([]/2).
:- arithmetic_function(init/2).       % initialize any variables
:- arithmetic_function(\\ /2).        % row concat
:- arithmetic_function(transpose/1).  % transpose array
% numeric functions
:- arithmetic_function(arange/2).     % new from range
:- arithmetic_function(arange/3).     % new from range
:- arithmetic_function(arange/4).     % new from range
:- arithmetic_function(identity/2).   % new NxN identity array
:- arithmetic_function(sum/1).        % sum of elements
:- arithmetic_function(min/1).        % minimum of elements
:- arithmetic_function(max/1).        % maximum of elements
:- arithmetic_function('.+'/2).       % numeric array addition
:- arithmetic_function('.-'/1).       % numeric array unary minus
:- arithmetic_function('.-'/2).       % numeric array subtraction
:- arithmetic_function('.*'/2).       % numeric array product
:- arithmetic_function('./'/2).       % numeric array division
:- arithmetic_function('.**'/2).      % numeric array power
:- arithmetic_function(apply/2).      % apply function/1 to array
:- arithmetic_function(cross/2).      % cross product of two 3D vectors
:- arithmetic_function(inner/2).      % inner product of two vectors
:- arithmetic_function(outer/2).      % outer product of two vectors
:- arithmetic_function(dot/2).        % dot product of two vectors/matrices
:- arithmetic_function(determinant/1).  % determinant of square matrix
:- arithmetic_function(inverse/1).    % inverse of square matrix

%
% Exports: filter for a N dimensional array
ndarray(Arr) :- ndarray_(Arr,_).

% for internal use, D is first dimension	
ndarray_(Arr,D) :- (compound(Arr) ; integer(D)), !, 
	functor(Arr,#,D).

%
% helper functions for N dimensional array
%

% create uninitialized
new_ndarray([D|Dn],Arr) :- integer(D), D>0,
	ndarray_(Arr,D),
	(Dn=[] -> true ; new_fill(D,Arr,Dn)).

new_fill(L,V,Dn) :-
	(L>0
	 ->	I is L-1,
		new_ndarray(Dn,Vs),
		[]([I],V,Vs),  %index_arr(V[I],Vs),  % V[I] is Vs[],  %%[]([I],V,Vs),
		new_fill(I,V,Dn)
	 ;	true
	).

% array dimensions (shape)
ndarray_dim([D|Dn],Arr) :-
	ndarray_(Arr,D), !,
	[]([0],Arr,Arr0),  %index_arr(Arr[0],Arr0),
	ndarray_dim(Dn,Arr0).
ndarray_dim([],_Arr).

% map to/from list form
map_ndarray(List,Arr) :- compound(Arr), Arr=..[#|Vals], !,
	map_elems_(List,Vals).
map_ndarray(List,Arr) :- is_list(List),
	map_elems_(List,Vals),
	Arr=..[#|Vals], !.
	
map_elems_([],[]).
map_elems_([E|Es],[E|Vs]) :- atomic(E), !,  % optimize
	map_elems_(Es,Vs).
map_elems_([E|Es],[V|Vs]) :- 
	(map_ndarray(E,V) -> true ; E=V),       % map sub-element of pass thru	
	map_elems_(Es,Vs).

%
% Function: create new array
%
new(ndarray,Dim,Arr) :- var(Arr), is_list(Dim), !,
	new_ndarray(Dim,Arr).
	
%
% Function: create new array from nested list
%
ndarray(List,Arr) :- is_list(List), List \= [],
	map_ndarray(List,Arr).
		
%
% Function: create new array from nested list
%
to_list(Arr,List) :- ndarray(Arr),
	map_ndarray(List,Arr).
		
%
% Function: indexing and slicing - basically uses vector indexing and slicing
%
[](Arr, Arr) :- 
	ndarray(Arr).
[]([B:E],Arr,R) :-                     % slicing evaluates to array
	ndarray_(Arr,Len),
	slice_parameters(B:E,Len,Start,SLen), SLen>0, !,
	ndarray_(R,SLen),
	sub_term(0,SLen,Start,Arr,R).
[]([Ix],Arr,R) :-                      % indexing evaluates to element
	ndarray_(Arr,Len),	
	index_parameters(Ix,Len,I),
	AIx is I+1,  % index_parameters are 0 based, arg is 1 based
	arg(AIx,Arr,R).

sub_term(Ix,Len,Start,From,To) :-      % like sub_string
	(Ix < Len
	 -> ToIx is Ix+1,
	 	FromIx is Start+ToIx,
		arg(FromIx,From,El),
		arg(ToIx,To,El),
		sub_term(ToIx,Len,Start,From,To)
	 ;  true
	).
	

%
% Function: shape
%
shape(Arr,S) :- ndarray(Arr),
	ndarray_dim(S,Arr).

%
% Function: number of dimensions (length of shape)
%
ndim(Arr,N) :- 
	shape(Arr,S),
	length(S,N).

%
% Function: number of elements (product of shape elements)
%
size(Arr,Sz) :- 
	shape(Arr,S),
	size_(S,1,Sz).
	
size_([],In,In).
size_([D|DN],In,Out) :-
	Nxt is D*In,
	size_(DN,Nxt,Out).

%
% Function: binds any vars to a value
%
init(Arr,Val,RArr) :-
	ndarray_(Arr,D),	
	fill_each(D,Arr,Val),
	RArr=Arr.

fill_each(N,Arr,Value) :-
	(N>0
	 ->	Ix is N-1,
	 	[]([Ix],Arr,Item),  %index_arr(Arr[Ix],Item),
		(ndarray_(Item,D)
		 -> fill_each(D,Item,Value)
		 ;	(var(Item) -> Item=Value ; true)
		),
		fill_each(Ix,Arr,Value)
	 ;	true
	).

%
% Function: row concat, dimensions > 0 must be the same
%    Note: Concat of two vectors is a single vector.
%          Use transpose (twice) for column concat
%
\\(A1, A2, AR) :-  ndarray(A1), ndarray(A2),
	ndarray_dim([R1|S],A1),
	ndarray_dim([R2|S],A2),
	RR is R1+R2,
	new_ndarray([RR|S],AR),
	AR[0:R1] is A1[],
	AR[R1:_] is A2[].
	
%
% Function: transpose (1D=noop, 2D=matrix transpose, 3+D=2D matrix of array)
%
transpose(Arr,TArr)	:- 
	shape(Arr,S),
	transpose_(S,Arr,TArr).

transpose_([C],Arr,TArr) :- !,       % 1D case result will be Cx1 array
	transpose1_([1,C],#(Arr),TArr).
transpose_([R,1|S],Arr,TArr) :- !,     % result will be a 1xR vector
	transpose1_([R,1|S],Arr,#(TArr)).
transpose_(Shp,Arr,TArr) :-          % general 2D case
	transpose1_(Shp,Arr,TArr).
	
transpose1_([R,C|S],Arr,TArr) :-
	new_ndarray([C,R|S],TArr),
	RIx is R-1,
	CIx is C-1,
	transpose_mtrx(CIx,RIx,RIx,Arr,TArr).

transpose_mtrx(C,R,Rows,Arr,TArr) :-
	V is Arr[R,C],
	V is TArr[C,R],
	(matrix_iterate(C,R,Rows,NxtC,NxtR) -> transpose_mtrx(NxtC,NxtR,Rows,Arr,TArr) ; true).

% Arithmetic functions on arrays, assumed to be numbers
%
% Function: arange
%
arange(ndarray,N,Arr) :- 
	Vs is arange(list,N),
	ndarray(Vs,Arr).

arange(ndarray,B,E,Arr) :- number(B), number(E),
	Vs is arange(list,B,E),
	ndarray(Vs,Arr).

arange(ndarray,B,E,S,Arr) :- number(B), number(E), number(S),
	Vs is arange(list,B,E,S),
	ndarray(Vs,Arr).
	
%
% Function: NxN identity matrix 
%
identity(ndarray,1,IdArr) :-  ndarray([1],IdArr).
identity(ndarray,N,IdArr) :-  integer(N), N>1,
	(var(IdArr) -> new(ndarray,[N,N],IdArr) ; true),
	Ix is N-1,
	diagonal_(Ix,IdArr),   % fill diagonal
	fill_each(N,IdArr,0).  % fill rest with 0
	

diagonal_(N,Arr) :-
	(N>=0
	 ->	Arr[N,N] is 1,
		Nxt is N-1,
		diagonal_(Nxt,Arr)
	 ;  true
	).

%
% Function: sum of array elements
%
sum(A,ASum) :- ndarray(A), !,
	ndarray_dim(Shp,A),
	sum_array(Shp,A,0,ASum).
	
sum_array([],N,Acc,R) :-
	R is Acc+N.  % final dimension, use accumulate number
sum_array([D|Ds],A,Acc,R) :-
	acc_subarray(D,sum_array,Ds,A,Acc,R).

%
% Function: min of array elements
%
min(A,AMin) :- ndarray(A), !,
	ndarray_dim(Shp,A),
	min_array(Shp,A,inf,AMin).
	
min_array([],N,Acc,R) :-
	R is min(Acc,N).  % final dimension, use min
min_array([D|Ds],A,Acc,R) :-
	acc_subarray(D,min_array,Ds,A,Acc,R).

%
% Function: max of array elements
%
max(A,AMax) :- ndarray(A), !,
	ndarray_dim(Shp,A),
	max_array(Shp,A,-inf,AMax).
	
max_array([],N,Acc,R) :-
	R is max(Acc,N).  % final dimension, use max
max_array([D|Ds],A,Acc,R) :-
	acc_subarray(D,max_array,Ds,A,Acc,R).

%
% Function: array addition
%
'.+'(N,A,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	add_arrays(Shp,N,A,AR).

'.+'(A,N,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	add_arrays(Shp,A,N,AR).

'.+'(A1,A2,AR) :- ndarray(A1), ndarray(A2),
	ndarray_dim(Shp,A1), ndarray_dim(Shp,A2),  % arrays have same shape
	new_ndarray(Shp,AR),
	add_arrays(Shp,A1,A2,AR).
	
add_arrays([],N1,N2,R) :- !, R is N1 + N2.  % final dimension, use numeric addition
add_arrays([D|Ds],A1,A2,AR) :-
	do_subarrays(D,add_arrays,Ds,A1,A2,AR).

%
% Function: array unary minus
%
'.-'(A,R) :- ndarray(A),
	'.*'(-1,A,R).  % multiply by -1

%
% Function: array subtraction 
%
'.-'(N,A,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	sub_arrays(Shp,N,A,AR).

'.-'(A,N,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	sub_arrays(Shp,A,N,AR).

'.-'(A1,A2,AR) :- ndarray(A1), ndarray(A2),
	ndarray_dim(Shp,A1), ndarray_dim(Shp,A2),  % arrays have same shape
	new_ndarray(Shp,AR),
	sub_arrays(Shp,A1,A2,AR).
	
sub_arrays([],N1,N2,R) :- !, R is N1 - N2.     % final dimension, use numeric subtraction
sub_arrays([D|Ds],A1,A2,AR) :-
	do_subarrays(D,sub_arrays,Ds,A1,A2,AR).

%
% Function: array multiplication
%
'.*'(N,A,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	mul_arrays(Shp,N,A,AR).

'.*'(A,N,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	mul_arrays(Shp,A,N,AR).

'.*'(A1,A2,AR) :- ndarray(A1), ndarray(A2),
	ndarray_dim(Shp,A1), ndarray_dim(Shp,A2),  % arrays have same shape
	new_ndarray(Shp,AR),
	mul_arrays(Shp,A1,A2,AR).
	
mul_arrays([],N1,N2,R) :- !, R is N1 * N2.     % final dimension, use numeric multiplication
mul_arrays([D|Ds],A1,A2,AR) :-
	do_subarrays(D,mul_arrays,Ds,A1,A2,AR).

%
% Function: array division
%
'./'(N,A,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	div_arrays(Shp,N,A,AR).

'./'(A,N,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	div_arrays(Shp,A,N,AR).

'./'(A1,A2,AR) :- ndarray(A1), ndarray(A2),
	ndarray_dim(Shp,A1), ndarray_dim(Shp,A2),  % arrays have same shape
	new_ndarray(Shp,AR),
	div_arrays(Shp,A1,A2,AR).
	
div_arrays([],N1,N2,R) :- !, R is N1 / N2.  % final dimension, use numeric division
div_arrays([D|Ds],A1,A2,AR) :-
	do_subarrays(D,div_arrays,Ds,A1,A2,AR).

%
% Function: array power
%
'.**'(N,A,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	pow_arrays(Shp,N,A,AR).

'.**'(A,N,AR) :- number(N), ndarray(A), !,
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	pow_arrays(Shp,A,N,AR).

'.**'(A1,A2,AR) :- ndarray(A1), ndarray(A2),
	ndarray_dim(Shp,A1), ndarray_dim(Shp,A2),  % arrays have same shape
	new_ndarray(Shp,AR),
	pow_arrays(Shp,A1,A2,AR).
	
pow_arrays([],N1,N2,R) :- !, R is N1 ** N2.  % final dimension, use numeric division
pow_arrays([D|Ds],A1,A2,AR) :-
	do_subarrays(D,pow_arrays,Ds,A1,A2,AR).
	
%
% Function: apply arithmetic function	
%
apply(F,A,AR) :- ndarray(A),
	ndarray_dim(Shp,A),
	new_ndarray(Shp,AR),
	apply_arrays(Shp,A,F,AR).

apply_arrays([],N,F,R) :- !,   % final dimension, apply function
	E=..[F,N], R is E.
apply_arrays([D|Ds],A,F,AR) :-
	apply_subarrays(D,apply_arrays,Ds,A,F,AR).

%
% Function: cross product of two 3 dimensional vectors	
%
cross(A1,A2,AR) :- ndarray(A1), ndarray(A2),
	ndarray_dim([3],A1), ndarray_dim([3],A2), !,  % two 3D vectors
	new_ndarray([3],AR),
	cross_(A1,A2,AR).
	
cross(A1,A2,R) :- ndarray(A1), ndarray(A2),
	ndarray_dim([2],A1), ndarray_dim([2],A2),     % two 2D vectors, convert to 3D
	new_ndarray([3],D3A1), new_ndarray([3],D3A2),
	D3A1[0:2] is A1[], D3A1[2] is 0,  % copy with Z axis = 0
	D3A2[0:2] is A2[], D3A2[2] is 0,
	new_ndarray([3],AR),	
	cross_(D3A1,D3A2,AR),
	vector3d(AR,_,_,R).               % result is Z (a scaler, X and Y are 0)

cross_(A1,A2,AR) :-                   % cross product of two 3D vectors
	vector3d(A1, A1_0,A1_1,A1_2),
	vector3d(A2, A2_0,A2_1,A2_2),
	vector3d(AR, AR_0,AR_1,AR_2),
	AR_0 is A1_1*A2_2 - A2_1*A1_2,
	AR_1 is A2_0*A1_2 - A1_0*A2_2,
	AR_2 is A1_0*A2_1 - A2_0*A1_1.

vector3d(#(N0,N1,N2),N0,N1,N2).

%
% Function: inner product of two vectors of same size	
%
inner(A1,A2,R) :- ndarray(A1), ndarray(A2),
	ndim(A1,1), ndim(A2,1),  % vectors only
	R is sum(A1 .* A2).      % fails if not same size

%
% Function: outer product of two vectors
%
outer(A1,A2,AR) :- ndarray(A1), ndarray(A2),
	ndarray_dim([S],A1),  % vector of size S
	ndarray_dim([_],A2),  % vector
	new_ndarray([S],R),
	Ix is S-1,
	outer_(Ix,A1,A2,R),
	((ndarray_dim([1,N],R), N>1) -> R = #(AR) ; R = AR).  % reduce if vector

outer_(Ix,A1,A2,AR) :- Ix >= 0,
	R is A1[Ix] .* A2[],
	(R = #(V) ->  AR[Ix] is V ; AR[Ix] is R[]),
	Nxt is Ix-1,
	(Nxt >= 0 -> outer_(Nxt,A1,A2,AR) ; true).

%
% Function: dot product of two arrays	
%
dot(A1,A2,AR) :- ndarray(A1), ndarray(A2),
	ndarray_dim(S1,A1),  % vector of same size S
	ndarray_dim(S2,A2),
	dot_(S1,S2,A1,A2,AR).
	
dot_([D], [D], A1, A2, AR) :-  !,        % vectors of same size, take inner product
	inner(A1,A2,AR).
dot_([D], [D,P], A1, A2, AR) :-  !,      % vector (A1), convert to matrix 
	dot_([1,D],[D,P],#(A1),A2,AR).
dot_([M,N], [N,P] , A1, A2, AR) :-  % matrix multiply
	new_ndarray([M,P],AR),
	transpose(A2,TA2),   % shape(TA2)  = [P,N]
	(P = 1 -> MA2 = #(TA2) ; MA2 = TA2),  % vector (A2), convert to matrix 
	MIx is M-1, PIx is P-1,
	matrix_mul(MIx,PIx,PIx,A1,MA2,AR).
	
matrix_mul(M,P,Rows,A1,A2,AR) :-
	AR[M,P] is inner(A1[M],A2[P]),
	(matrix_iterate(M,P,Rows,NxtM,NxtP) -> matrix_mul(NxtM,NxtP,Rows,A1,A2,AR) ; true).

%
% Function: determinant of a square matrix	
%
determinant(A,Det) :- ndarray(A),
	ndarray_dim(S,A),
	determinant_(S,A,Det).

determinant_([1],A,Det) :- 
	[]([0],A,Det).  %index_arr(A[0],Det).
determinant_([2,2],A,Det) :-
	A = #( #(A_00,A_01) , #(A_10,A_11) ),
	Det is A_00*A_11 - A_01*A_10.
determinant_([N,N],A,Det) :-
	map_ndarray([Row|Rest],A),  % convert to list form, use top row
	MN is N-1,                  % minor dimension
	determinant_(0,N,MN,Row,Rest,0,Det).
	
determinant_(I,N,MN,Row,Rest,Acc,Det) :-
	(I<N
	 ->	El is Row[I],
		minor_determinant_(Rest,I,MN,MDet),
		NxtA is Acc + El*(-1**I)*MDet,
		NxtI is I+1,
		determinant_(NxtI,N,MN,Row,Rest,NxtA,Det)
	 ;	Det=Acc
	).

minor_determinant_(List,I,2,Det) :- !,
	minor(List,I,[ [A_00,A_01], [A_10,A_11] ]),
	Det is A_00*A_11 - A_01*A_10.		
minor_determinant_(List,I,MN,Det) :-
	minor(List,I,[Row|Rest]),      % list form of minor
	MMN is MN-1,
	determinant_(0,MN,MMN,Row,Rest,0,Det).	

minor([],_I,[]).
minor([X|Xs],I,[M|Ms]) :-
	delete_item(I,X,M),
	minor(Xs,I,Ms).
	
delete_item(0,[_|Xs],Xs) :- !.
delete_item(I,[X|Xs],[X|Ms]) :-
	Nxt is I-1,	
	delete_item(Nxt,Xs,Ms).

%
% Function: inverse of a square matrix	
%
inverse(A,Inv) :- ndarray(A),
	ndarray_dim(S,A),
	inverse_(S,A,Inv).
	
inverse_([1],A,Inv) :- !,
	A0 is A[0],
	zero_div_chk(1,A0,IN), 
	ndarray([IN],Inv).
inverse_([2,2],A,Inv) :- !,     % special case 2x2 to avoid vector edge cases in NxN
	determinant_([2,2],A,ADet), 
	zero_div_chk(1,ADet,IDet),  % may fail if determinant=0
	A = #( #(A_00,A_01) , #(A_10,A_11) ),
	I_00 is A_11*IDet, I_01 is -A_01*IDet,
	I_10 is -A_10*IDet, I_11 is A_00*IDet,
	ndarray([[I_00, I_01], [I_10, I_11]],Inv).
inverse_([N,N],A,Inv) :-
	determinant_([N,N],A,ADet), 
	zero_div_chk(1,ADet,IDet),  % may fail if determinant=0
	new_ndarray([N,N],Inv),
	map_ndarray(AList,A),       % use list form for minor/3
	D is N-1,  % for indexing
	inverse_iterate_(D,D,D,IDet,AList,Inv).
	
inverse_iterate_(R,C,D,IDet,AList,Inv) :-
	sub_array_(R,D,AList,Sub),
	minor_determinant_(Sub,C,D,SDet),
	Inv[C,R] is -1**(R+C)*SDet*IDet,  % single calculation includes transpose & cofactors
	(matrix_iterate(C,R,D,NxtC,NxtR)
	 -> inverse_iterate_(NxtR,NxtC,D,IDet,AList,Inv)
	 ;  true
	).
	
sub_array_(0,_,A,Sub) :- !,
	Sub is A[1:_].
sub_array_(D,D,A,Sub) :- !,
	Sub is A[0:D].
sub_array_(R,_,A,Sub) :-
	R1 is R+1,
	Sub is A[0:R]\\A[R1:_].

zero_div_chk(N,D,R)      :- D=\=0, !, R is N/D.
zero_div_chk(_,_,1.0Inf) :- current_prolog_flag(float_zero_div,infinity).

%
% helpers for iterating through nested array levels
%
do_subarrays(N,Op,Ds,V1,V2,VR) :-
	(N>0
	 ->	I is N-1,
		(number(V1) -> V1i=V1 ; []([I],V1,V1i)),  %index_arr(V1[I],V1i)),  %  VS1 is V1[I]),  % simple "broadcast" 
		(number(V2) -> V2i=V2 ; []([I],V2,V2i)),  %index_arr(V2[I],V2i)),  %  VS2 is V2[I]),
		[]([I],VR,VRi),  % index_arr(VR[I],VRi),  % VSR is VR[I],
		SubOp =.. [Op,Ds,V1i,V2i,VRi], SubOp,   % SubOp(Ds,VS1,VS2,VSR)
		do_subarrays(I,Op,Ds,V1,V2,VR)
	 ;	true
	).

apply_subarrays(N,Op,Ds,V,F,VR) :-
	(N>0
	 ->	I is N-1,
		(number(V) -> Vi=V ; []([I],V,Vi)),  % index_arr(V[I],Vi)),  %  VS is V[I]),  % simple "broadcast" 
		[]([I],VR,VRi),  % index_arr(VR[I],VRi),  % VSR is VR[I],
		SubOp =.. [Op,Ds,Vi,F,VRi], SubOp,          % SubOp(Ds,VS1,VS2,VSR)
		apply_subarrays(I,Op,Ds,V,F,VR)
	 ;	true
	).

acc_subarray(N,Op,Ds,V,Acc,R) :-
	(N>0
	 ->	I is N-1,
		(number(V) -> Vi=V ; []([I],V,Vi)),  % index_arr(V[I],Vi)),  %  VS is V[I]),        % simple "broadcast" 
		SubOp =.. [Op,Ds,Vi,Acc,AccR], SubOp,      % SubOp(Ds,VS1,VS2,VSR)
		acc_subarray(I,Op,Ds,V,AccR,R)
	 ;	R=Acc
	).

matrix_iterate(C,0,Rows,NxtC,Rows) :- !, C>0,
	NxtC is C-1.
matrix_iterate(C,R,_Rows,C,NxtR) :- R>0,
	NxtR is R-1.	
