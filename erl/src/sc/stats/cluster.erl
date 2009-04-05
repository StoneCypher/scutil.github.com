
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date$</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks

%% @todo parameterize on sc.space modules





-module(sc.stats.cluster).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Clustering mechanisms for the statistics package").

-testerl_export( { [], sc_stats_cluster_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [
    centroid/1
] ).




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

    [ .sc.stats.means:arithmetic_mean(X) ||
        X <- .sc.list:zip_n(CoordList, to_list)
    ].
