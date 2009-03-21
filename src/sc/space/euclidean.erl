




%% @spec euclidean_distance(Coordinate1::coord(), Coordinate2::coord()) -> number()

%% @doc {@section Math} Returns the distance between two coordinates in any N-space.  In two dimensions, this is known as the Pythagorean theorem.  The coordinates may be of any positive integer dimensionality (2d, 3d, but no -1d or 2.5d), but both coordinates must be of the same dimensionality.  The coordinates may have real-valued or negative components, but imaginary math is not implemented.  This function tolerates tuple coordinates by converting them to lists; list coordinates are thus slightly faster. ```1> scutil:euclidean_distance([0,0],[1,1]).
%% 1.4142135623730951
%%
%% 2> scutil:euclidean_distance({0,0},[-1,1.0]).
%% 1.4142135623730951
%%
%% 3> scutil:euclidean_distance([0,0,0,0],[1,-1,1,-1]).
%% 2.0'''

%% @since Version 108

euclidean_distance(C1, C2) when is_tuple(C1) -> 

    euclidean_distance( tuple_to_list(C1), C2 );



euclidean_distance(C1, C2) when is_tuple(C2) ->

    euclidean_distance( C1, tuple_to_list(C2) );



euclidean_distance(C1, C2) ->

    % squaring makes taking the absolute value to get unsigned magnitude redundant; that's not an omission, it's an optimization
    math:sqrt(lists:sum([ square(A-B) || {A,B} <- scutil:zip_n([C1,C2]) ])).
