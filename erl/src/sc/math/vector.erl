
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision: 264 $
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-03-15 13:47:01 -0600 (Sun, 15 Mar 2009) $</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(sc.math.vector).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id: vector.erl 264 2009-03-23 05:49:11Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/src/sc/math/vector.erl $").
-svn_revision("$Revision: 264 $").

-description("Vector calculations for math, physics and 3d.  This means vector in the physics sense, not in the datastructure sense.").

-testerl_export( { [], sc_vector_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [
    qsp_average/2,
    dot_product/2,
    cross_product/2
] ).





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

    .sc.stats.means:arithmetic_mean([ GetSqVnDp(Xi) || Xi <- InputVecs ]).





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

    .lists:sum( [ X*Y || {X,Y} <- .lists:zip(VX,VY) ] ).





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





%% @equiv root_sum_square(VX)

%% @doc {@section Math} Returns the magnitude of a vector.  A vector's magnitude is the length of its hypoteneuse (and is as such the root sum square of its components).  A vector can be seen as the product of its unit vector and its magnitude; as such many people see a vector's magnitude as its scale. ```1> scutil:vector_magnitude([0,0,0]).
%% 0.0
%%
%% 2> scutil:vector_magnitude([1,0,0]).
%% 1.0
%%
%% 3> scutil:vector_magnitude([1,1,1]).
%% 1.7320508075688772
%%
%% 4> scutil:vector_magnitude([1,2,3]).
%% 3.7416573867739413
%%
%% 5> scutil:vector_magnitude([0,0.4,0.6,0.2,0.4,0.529150262213]).
%% 1.0000000000000433'''

%% @since Version 85

vector_magnitude(VX) ->

    root_sum_square(VX).





%% @type unit_vector() = vector().  The hypoteneuse of a unit vector is precisely one unit long.  Unit vectors are also called normalized or magnitude-normalized vectors.

%% @spec normalize_vector(Vector::vector()) -> unit_vector()

%% @doc {@section Math} Returns the magnitude of a vector.  A vector's magnitude is the length of its hypoteneuse.  A vector can be seen as the product of its unit vector and its magnitude; as such many people see a vector's magnitude as its scale.  The normal of the zero vector is undefined, in the way that dividing by zero is undefined, and will throw an arithmetic exception. ```1> scutil:normalize_vector([0,3,4]).
%% [0.0,0.6,0.8]'''<span style="color:red">TODO: When tuple comprehensions are introduced to the language, convert this to using them.</span>

%% @since Version 85

normalize_vector(VX) when is_list(VX) ->
    
    VM = vector_magnitude(VX),
    [ X / VM || X <- VX ];



normalize_vector(VX) when is_tuple(VX) -> 

    list_to_tuple(normalize_vector(tuple_to_list(VX))).
