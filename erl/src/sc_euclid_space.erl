
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-04-05 16:16:32 -0600 (Sun, 05 Apr 2009) $</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(sc_euclid_space).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Euclidean distance metrics").

-testerl_export( { [], sc_math_space_euclidean_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    distance/2

] ).





%% @spec distance(Coordinate1::coord(), Coordinate2::coord()) -> number()

%% @doc {@section Math} Returns the distance between two coordinates in any N-space.  In two dimensions, this is known as the Pythagorean theorem.  The coordinates may be of any positive integer dimensionality (2d, 3d, but no -1d or 2.5d), but both coordinates must be of the same dimensionality.  The coordinates may have real-valued or negative components, but imaginary math is not implemented.  This function tolerates tuple coordinates by converting them to lists; list coordinates are thus slightly faster. ```1> sc.math.space.euclidean:distance([0,0],[1,1]).
%% 1.4142135623730951
%%
%% 2> sc.math.space.euclidean:distance({0,0},[-1,1.0]).
%% 1.4142135623730951
%%
%% 3> sc.math.space.euclidean:distance([0,0,0,0],[1,-1,1,-1]).
%% 2.0'''

%% @since Version 108

% was scutil:euclidean_distance/2

distance(C1, C2) when is_tuple(C1) ->

    distance( tuple_to_list(C1), C2 );



distance(C1, C2) when is_tuple(C2) ->

    distance( C1, tuple_to_list(C2) );



distance(C1, C2) ->

    % squaring makes taking the absolute value to get unsigned magnitude redundant; that's not an omission, it's an optimization
    math:sqrt(
        lists:sum(
            [ sc_math:square(A-B) ||
                {A,B} <- sc_lists:zip_n([C1,C2])
            ]
        )
    ).
