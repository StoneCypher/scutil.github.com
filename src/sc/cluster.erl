
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <p></p>


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

%% @todo parameterize on sc.space modules




% todo

% group_by_distance(CenterList, [],           Work) -> lists:reverse(Work);
% group_by_distance(CenterList, [Coord|RemC], Work) ->

%   zip_n([ [ {CoordId,CenterId,euclidean_distance(Coord, Center)} || {CoordId,Coord} <- lists:zip(lists:seq(1,length(CoordList)),CoordList) ] || {CenterId,Center} <- lists:zip(lists:seq(1,length(CenterList)),CenterList) ]).





% todo

% k_means(CoordList) when is_list(CoordList) ->





% convenient in list comprehensions

%% @spec centroid(InputList::coord_list()) -> coord()

%% @doc {@section Statistics} Calculates the coordinate which represents the per-axis arithmetic mean of a set of points.  To calculate the centroid of `[1,1]', `[2,3]', you gather the X coordinates `[1,2]', then use their mean `1.5'; then do the same for the Y, `[1,3]' to `2'.  The centroid would thus be `[1.5,2]'.  You may pass any number of coordinates to this function, of any axis count, but they must all be the same axis count.  The return value will be a coordinate with the same axis count.  Negative and real values are fine; imaginary math is not implemented. ```1> scutil:centroid([[1]]).
%% [1.0]
%%
%% 2> scutil:centroid([[1,1],[2,2]]).
%% [1.5,1.5]
%%
%% 3> scutil:centroid([[1,1,1],[2,2,2],[3,3,3]]).
%% [2.0,2.0,2.0]
%%
%% 4> scutil:centroid([[1,-1,1.0],[-2,-2,-2],[3,3,3],[4,4,4],[5,5,5]]).
%% [2.2,1.8,2.2]'''

%% @since Version 118

centroid(CoordList) when is_list(CoordList) -> 

    [ arithmetic_mean(X) || 
        X <- zip_n(CoordList, to_list)
    ].
