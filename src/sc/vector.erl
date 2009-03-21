




% quadratic scalar product average
% see http://www.inf.fu-berlin.de/inst/ag-ki/rojas_home/documents/1996/NeuralNetworks/K5.pdf pdf-page 15
% Thanks to the following for help with qsp_average and dependencies: Asterick, Chile, John Sensebe, PfhorSlayer, Raleigh

%% @type vector() = list() | tuple().  Every member element of a vector() is a {@type number()}.
%% @type vectorlist() = list().  Every member element of a vectorlist() is a {@type vector()}.

%% @spec qsp_average(W::numericlist(), InputVecs::vectorlist()) -> float()

%% @doc {@section Math} Takes the quadratic scalar product average of a vector `W' and a list of vectors `X'.  The QSP Average
%% is the arithmetic mean of the result set Y, where Y is generated as the square of the magnitude of the dot product
%% of W and each individual vector in X.```1> scutil:qsp_average([1,2,3], [[0,0,0],[0,0,0]]).
%% 0.0
%%
%% 2> scutil:qsp_average([1,2,3], [[0,0,1],[0,0,0]]).
%% 4.5
%%
%% 3> scutil:qsp_average([1,2,3], [[0,1,0],[0,0,0]]).
%% 2.0
%%
%% 4> scutil:qsp_average([1,2,3], [[1,0,0],[0,0,0]]).
%% 0.5
%%
%% 5> scutil:qsp_average([1,2,3], [[1,1,1],[0,0,0]]).
%% 18.0
%%
%% 6> scutil:qsp_average([1,2,3], [[0,0,0],[1,1,1]]).
%% 18.0
%%
%% 7> scutil:qsp_average([1,2,3], [[1,1,1],[1,1,1]]).
%% 36.0'''The linked documentation incorrectly uses the notation ||Foo|| instead of |Foo| to
%% present the algorithm.  ||Foo|| is the vector magnitude - the root sum square of vector elements - but as the input is the
%% dot product of two 1d vectors, which will always be a single number, the vector magnitude serves no purpose other than to
%% normalize the sign slowly and counterintuitively; thus we switch to abs despite the documentation.  {@section Thanks} to Steve
%% Stair for helping straighten this out.

%% @since Version 82

qsp_average(W, InputVecs) ->

    GetSqVnDp = fun(Xi) ->
        VnDp = abs(dot_product(W, Xi)),
        VnDp * VnDp
    end,

    arithmetic_mean([ GetSqVnDp(Xi) || Xi <- InputVecs ]).





% removed when length(VX) == length(VY) because it's implied by lists:zip

%% @spec dot_product(VX::numeric_list(), VY::numeric_list()) -> number()

%% @doc {@section Math} <span style="color:red">Incomplete</span> Calculates the dot product of two vectors (<span style="color:red">Incomplete</span> represented as numeric lists; tuples not yet supported). ```1> scutil:dot_product([1,1,1],[2,2,2]).
%% 6
%%
%% 2> scutil:dot_product([1,1,1],[3,3,3]).
%% 9
%%
%% 3> scutil:dot_product([-1,0,1],[3,3,3]).
%% 0
%%
%% 4> scutil:dot_product([-1,1,1],[3,3,3]).
%% 3
%%
%% 5> scutil:dot_product([0.5,1,2],[1,1,1]).
%% 3.5'''<span style="color:red">TODO: The tuple variation of vectors has not yet been implemented in this function.</span>

%% @since Version 80

%% @todo implement tuple variation

dot_product(VX, VY) ->
    
    lists:sum( [ X*Y || {X,Y} <- lists:zip(VX,VY) ] ).





%% @type three_vector() = vector().  A three-vector always has three elements, so this can be expressed as the alternation `{A::number(), B::number(), C::number()} | [A::number(), B::number(), C::number()]'.
%% @type seven_vector() = vector().  A seven-vector always has seven elements, so this can be expressed as the alternation `{A::number(), B::number(), C::number(), D::number(), E::number(), F::number(), G::number()} | [A::number(), B::number(), C::number(), D::number(), E::number(), F::number(), G::number()]'.
%% @type three_or_seven_vector() = three_vector() | seven_vector().

%% @spec cross_product(VX::three_vector(), VY::three_vector()) -> three_vector()

%% @doc {@section Math} <span style="color:red">Incomplete</span> Calculates the cross product of two vectors (<span style="color:red">Incomplete</span> represented as {@type three_vector()}s - no support yet for seven). ```1> scutil:dot_product([1,1,1],[2,2,2]).
%% 6
%%
%% 2> scutil:dot_product([1,1,1],[3,3,3]).
%% 9
%%
%% 3> scutil:dot_product([-1,0,1],[3,3,3]).
%% 0
%%
%% 4> scutil:dot_product([-1,1,1],[3,3,3]).
%% 3
%%
%% 5> scutil:dot_product([0.5,1,2],[1,1,1]).
%% 3.5'''<span style="color:red">TODO: Implement seven-dimensional cross product</span>

%% @since Version 80

%% @todo implement 7-dimensional variation, http://en.wikipedia.org/wiki/Seven-dimensional_cross_product

cross_product( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->
    
    { (Y1*Z2) - (Z1*Y2) , (Z1*X2) - (X1*Z2), (X1*Y2) - (Y1*X2) };



cross_product( [X1,Y1,Z1], [X2,Y2,Z2] ) ->

    [ (Y1*Z2) - (Z1*Y2) , (Z1*X2) - (X1*Z2), (X1*Y2) - (Y1*X2) ].
