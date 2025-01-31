/*	The MIT License (MIT)
 *
 *	Copyright (c) 2021-25 Rick Workman
 *
 *	Permission is hereby granted, free of charge, to any person obtaining a copy
 *	of this software and associated documentation files (the "Software"), to deal
 *	in the Software without restriction, including without limitation the rights
 *	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *	copies of the Software, and to permit persons to whom the Software is
 *	furnished to do so, subject to the following conditions:
 *
 *	The above copyright notice and this permission notice shall be included in all
 *	copies or substantial portions of the Software.
 *
 *	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *	SOFTWARE.
 */
:- module(type_ndarray,
	[ ndarray/1,
	  op(500, yfx, '.+'),      % arithmetic operators on arrays
	  op(200, fy,  '.-'),      % same precedence and associativity as standard ops
	  op(500, yfx, '.-'),
	  op(400, yfx, '.*'),
	  op(400, yfx, './') , 
	  op(200, xfx, '.**')  
	]).

%:- reexport(library(type_list),[op(_,_,_)]).  % requires fix circa 9.3.19 % for operators
:- reexport(library(type_list),except([slice_parameters/4,index_parameters/3])).  % for operators

/** <module> arithmetic type `ndarray` (mulitidimensional array)

This module implements arithmetic type (see module `arithmetic_types) `ndarry`, immutable, N-dimensional arrays largely patterned after Pyhon's **NumPy** library. Support from slicing and indexing (0 based) are imported from module `type_list`. A one dimensional array is represented a compound term with functor `#` and arguments being the elements of the array. N-dimensional arrays are supported by allowing arrays as array elements. The elements of the array can be any Prolog term, although a subset of the defined arithmetic functions on `ndarray`'s only succeed if the elements are numbers. (There is one exception described below.)
 
The only exported predicate is the type checking `ndarray/1`. Also exported are a set of array arithmetic operations: `.+, .-, .*,` etc.; refer ot source for a complete list. The set of arithmetic functions defined by this module include:
```
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
:- arithmetic_function(flatten/1).    % flattened array
:- arithmetic_function(reshape/2).    % reshaped array
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
```

`type_ndarray` supports the use of constrained numeric values as defined by `library(clpBNR)` If `current_module(clpBNR)` succeeds (i.e., the module has been loaded), instantiation errors from standard functional arithmetic will be interpreted as numeric constraints on the variable(s) in the sub-expression being evaluated. If `clpBNR` is not accessible, the original instantiation error will be propagated.
 
See the ReadMe for this pack for more documentation and examples.
*/
:- use_module(library(arithmetic_types)).
% for indexing and slicing support, arange/n content
:- use_module(library(type_list)).

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
%- arithmetic_function(flatten/1).    % flattened array (directive below)
:- arithmetic_function(reshape/2).    % reshaped array
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

/** 
ndarray(?X:array) is semidet

Succeeds if X is an array; otherwise fails.
*/
% definition of a N dimensional array
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
	 ;  true
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
[]([B:E],Arr,R) :-                   % slicing evaluates to array
	ndarray_(Arr,Len),
	slice_parameters(B:E,Len,Start,SLen), SLen>0, !,
	ndarray_(R,SLen),
	sub_term(0,SLen,Start,Arr,R).
[]([Ix],Arr,R) :-                      % indexing evaluates to element
	ndarray_(Arr,Len),
	index_parameters(Ix,Len,I),
	AIx is I+1,  % index_parameters are 0 based, arg is 1 based
	arg(AIx,Arr,R).

sub_term(Ix,Len,Start,From,To) :-
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
	 -> Ix is N-1,
	    []([Ix],Arr,Item),  %index_arr(Arr[Ix],Item),
	    (ndarray_(Item,D)
	     -> fill_each(D,Item,Value)
	     ;  (var(Item) -> Item=Value ; true)
	    ),
	    fill_each(Ix,Arr,Value)
	 ;  true
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
% Function: flatten
%
flatten(Arr,FArr) :-
	% equivalent to:  size(Arr,S), reshape(Arr,[S],FArr)
	FList is flatten(to_list(Arr)),               % flattened input as list
	FArr =.. [#|FList].                           % new array from flat data

:- arithmetic_function(flatten/1).    % flattened array from local flatten/2

%
% Function: reshape
%
reshape(Arr,NShp,NArr) :- 
	new_ndarray(NShp,NArr),                       % target array of new shape
	length(NShp,N), length(Ixs,N), init_Ix(Ixs),  % starting indices for copy  
	FList is flatten(to_list(Arr)),               % flatten input array to list
	copy_list(Ixs,NShp,FList,[],NArr).            % copy data in list to target

init_Ix([]).
init_Ix([0|Ix]) :- init_Ix(Ix). 

copy_list([Ix],[D],[Val|Vals],NVals,NArr) :- !,
	[]([Ix],NArr,Val),
	NIx is Ix+1,
	(NIx < D
	 -> copy_list([NIx],[D],Vals,NVals,NArr)
	 ;  NVals = Vals
	).
copy_list([Ix|Ixs],[D|Ds],Vals,NVals,NArr) :-
	[]([Ix],NArr,NxtArr),
	copy_list(Ixs,Ds,Vals,NxtVals,NxtArr),
	NIx is Ix+1,
	(NIx < D
	 -> copy_list([NIx|Ixs],[D|Ds],NxtVals,NVals,NArr)
	 ; NVals = NxtVals
	).

%
% Function: transpose
%
transpose(Arr,TArr)	:- 
	shape(Arr,S),
	(length(S,1)
	 -> TArr = Arr               % 1D case, numpy semantics (T.shape = reverse(A.shape)
	 ;  bagof((Path,Element),Element^arr_elements(S,Arr,Path/Path,Element),AllEl),  % collect all (Path,Value) in Arr
	    lists:reverse(S,TS),                                 % transposed shape is reverse
	    (shape(TArr,TS) -> true ; TArr is new(ndarray,TS)),  % target for transposed 
	    transpose_elements(AllEl,TArr)                       % copy values to transposed
	).

% backtracking generator of (Path,Value) pairs, collected with `bagof`
arr_elements([],Element,_Path/[],Element).
arr_elements([S|Shp],Arr,Path/[Ix|Nxt],Element) :-
	MaxI is S-1,
	between(0,MaxI,Ix),          % actual generator
	[]([Ix],Arr,SubArr),
	arr_elements(Shp,SubArr,Path/Nxt,Element).

% update transposed using list of (Path,Value) pairs
transpose_elements([],_TArr).
transpose_elements([(Path,Val)|Els],TArr) :-
	to_array(Path,TArr,Val),     % uses reverse path to update transposed
	transpose_elements(Els,TArr).
	
to_array([P],TArr,Val) :- !, 
	[]([P],TArr,Val).            % the end, start here
to_array([P|Path],TArr,Val) :-
	to_array(Path,TArr,SubArr),  % work backwards from the end
	[]([P],SubArr,Val).

% Arithmetic functions on arrays, assumed to be numbers
%
% Function: arange
%
arange(ndarray,N,Arr) :- 
	Vs is arange(list,N),      % Vs is flat
	Arr =.. [#|Vs].

arange(ndarray,B,E,Arr) :- number(B), number(E),
	Vs is arange(list,B,E),    % Vs is flat
	Arr =.. [#|Vs].

arange(ndarray,B,E,S,Arr) :- number(B), number(E), number(S),
	Vs is arange(list,B,E,S),  % Vs is flat
	Arr =.. [#|Vs].

%
% Function: NxN identity matrix 
%
identity(ndarray,1,#(1)).
identity(ndarray,N,IdArr) :-  integer(N), N>1,
	(var(IdArr) -> new(ndarray,[N,N],IdArr) ; true),
	Ix is N-1,
	diagonal_(Ix,IdArr),   % fill diagonal
	fill_each(N,IdArr,0).  % fill rest with 0

diagonal_(N,Arr) :-
	(N>=0
	 -> Arr[N,N] is 1,
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
	catch(add(Acc,N,R),Err, constrain(Err,R is Acc+N)).  % final dimension, use accumulate number
sum_array([D|Ds],A,Acc,R) :-
	acc_subarray(D,sum_array,Ds,A,Acc,R).

%
% Function: min of array elements
%
min(A,AMin) :- ndarray(A), !,
	ndarray_dim(Shp,A),
	min_array(Shp,A,inf,AMin).

min_array([],N,Acc,R) :-
	catch(min(Acc,N,R), Err, constrain(Err,R is min(Acc,N))).  % final dimension, use min
min_array([D|Ds],A,Acc,R) :-
	acc_subarray(D,min_array,Ds,A,Acc,R).

%
% Function: max of array elements
%
max(A,AMax) :- ndarray(A), !,
	ndarray_dim(Shp,A),
	max_array(Shp,A,-inf,AMax).
	
max_array([],N,Acc,R) :-
	catch(max(Acc,N,R),Err, constrain(Err,R is max(Acc,N))).  % final dimension, use max
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
	
add_arrays([],N1,N2,R) :- !,
	catch(add(N1,N2,R), Err, constrain(Err, R is N1 + N2)).  % final dimension, use numeric addition
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

sub_arrays([],N1,N2,R) :- !,
	catch(sub(N1,N2,R), Err, constrain(Err, R is N1 - N2)).  % final dimension, use numeric subtraction
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

mul_arrays([],N1,N2,R) :- !,
	catch(mul(N1,N2,R), Err, constrain(Err, R is N1 * N2)).  % final dimension, use numeric multiplication
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

div_arrays([],N1,N2,R) :- !,
	catch(div(N1,N2,R), Err, constrain(Err, R is N1 / N2)).  % final dimension, use numeric division
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

pow_arrays([],N1,N2,R) :- !,
	catch(pow(N1,N2,R), Err, constrain(Err, R is N1 ** N2)).  % final dimension, use numeric power
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
	E=..[F,N], 
	catch(R is E, Err, constrain(Err, R is E)).
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
	catch(det2x2(A1_1,A1_2,A2_1,A2_2,AR_0), Err0, constrain(Err0, AR_0 is A1_1*A2_2 - A2_1*A1_2)),
	catch(det2x2(A2_0,A2_2,A1_0,A1_2,AR_1), Err1, constrain(Err1, AR_1 is A2_0*A1_2 - A1_0*A2_2)),
	catch(det2x2(A1_0,A1_1,A2_0,A2_1,AR_2), Err2, constrain(Err2, AR_2 is A1_0*A2_1 - A2_0*A1_1)).

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
dot_([M,N], [N,P] , A1, A2, AR) :-       % matrix multiply
	new_ndarray([M,P],AR),
	transpose(A2,TA2),   % shape(TA2)  = [P,N]
	MIx is M-1, PIx is P-1,
	matrix_mul(MIx,PIx,PIx,A1,TA2,AR).

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
	catch(det2x2(A_00,A_01,A_10,A_11,Det), Err, constrain(Err, Det is A_00*A_11 - A_01*A_10)).
determinant_([N,N],A,Det) :-
	map_ndarray([Row|Rest],A),  % convert to list form, use top row
	MN is N-1,                  % minor dimension
	determinant_(0,N,MN,Row,Rest,0,Det).

determinant_(I,N,MN,Row,Rest,Acc,Det) :-
	(I<N
	 ->	El is Row[I],
	 	(number(El), El =:= 0
	 	 -> NxtA = Acc  % element is 0, no use calculating determinant
	 	 ;	minor_determinant_(Rest,I,MN,MDet),
 			I1 is 1 - 2*(I mod 2), % alternating 1 and -1 
	 	 	catch(acc_det(Acc,El,I1,MDet,NxtA), Err, constrain(Err, NxtA is Acc + El*I1*MDet))
	 	),
		NxtI is I+1,
		determinant_(NxtI,N,MN,Row,Rest,NxtA,Det)
	 ;	Det=Acc
	).

minor_determinant_(List,I,2,Det) :- !,
	minor(List,I,[ [A_00,A_01], [A_10,A_11] ]),
	catch(det2x2(A_00,A_01,A_10,A_11,Det), Err, constrain(Err, Det is A_00*A_11 - A_01*A_10)).
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
	catch(inv(A0,IN), Err, constrain(Err, IN is 1/A0)),  % fails if det=0
	ndarray([IN],Inv).
inverse_([2,2],A,Inv) :- !,     % special case 2x2 to avoid vector edge cases in NxN
	determinant_([2,2],A,ADet),
	catch(inv(ADet,IDet), Err, constrain(Err, IDet is 1/ADet)),  % fails if det=0
	A = #( #(A_00,A_01) , #(A_10,A_11) ),
	catch(mul(A_11,IDet,I_00), Err00, constrain(Err00,I_00 is  A_11*IDet)),
	catch(mml(A_01,IDet,I_01), Err01, constrain(Err01,I_01 is -A_01*IDet)),
	catch(mml(A_10,IDet,I_10), Err10, constrain(Err10,I_10 is -A_10*IDet)),
	catch(mul(A_00,IDet,I_11), Err11, constrain(Err11,I_11 is  A_00*IDet)),
	ndarray([[I_00, I_01], [I_10, I_11]],Inv).
inverse_([N,N],A,Inv) :-
	determinant_([N,N],A,ADet), 
	catch(inv(ADet,IDet), Err, constrain(Err, IDet is 1/ADet)),  % fails if det=0
	new_ndarray([N,N],Inv),
	map_ndarray(AList,A),       % use list form for minor/3
	D is N-1,  % for indexing
	inverse_iterate_(D,D,D,IDet,AList,Inv).

inverse_iterate_(R,C,D,IDet,AList,Inv) :-
	sub_array_(R,D,AList,Sub),
	minor_determinant_(Sub,C,D,SDet),
	X is Inv[C,R],             % inverted array element (transpose(A[R,C])
	Sgn is 1-2*((R+C)mod 2),   % alternating -1,1	
	catch(mul3(Sgn,SDet,IDet,X), Err, constrain(Err, X is Sgn*SDet*IDet)),
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
		(number(V) -> Vi=V ; []([I],V,Vi)),        % simple "broadcst" 
		SubOp =.. [Op,Ds,Vi,Acc,AccR], SubOp,      % SubOp(Ds,VS1,VS2,VSR)
		acc_subarray(I,Op,Ds,V,AccR,R)
	 ;	R=Acc
	).

%
% arithmetic --> clpBNR constraint
%
constrain(error(instantiation_error,_), Goal) :-
	current_module(clpBNR),            % clpBNR constraints available?
	Mod=clpBNR, Mod:constrain_(Goal),  % can't use module name directly as it invalidates current_module test
	!.
constrain(Err, _Goal) :-
	throw(Err).

%
% Compiled arithmetic (Note: adding/subtracting a constant already optimal)
%
:- set_prolog_flag(optimise,true).

matrix_iterate(C,0,Rows,NxtC,Rows) :- !, C>0,
	NxtC is C-1.
matrix_iterate(C,R,_Rows,C,NxtR) :- R>0,
	NxtR is R-1.

add(N1,N2,R) :- R is  N1+N2.
sub(N1,N2,R) :- R is  N1-N2.
mul(N1,N2,R) :- R is  N1*N2.
mml(N1,N2,R) :- R is -N1*N2.
div(N1,N2,R) :- R is  N1/N2.
pow(N1,N2,R) :- R is  N1**N2.
max(N1,N2,R) :- R is  max(N1,N2).
min(N1,N2,R) :- R is  min(N1,N2).
mul3(N1,N2,N3,R) :- R is N1*N2*N3.
inv(N,R)     :- N=\=0, R is 1/N.
det2x2(A0_0,A0_1,A1_0,A1_1,R) :- R is A0_0*A1_1 - A0_1*A1_0.
acc_det(Acc,El,I1,MDet,NxtA)  :- NxtA is Acc + El*I1*MDet.
