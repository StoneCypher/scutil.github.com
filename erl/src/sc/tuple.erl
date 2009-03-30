
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision: 263 $
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





-module(sc.tuple).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id: tuple.erl 263 2009-03-23 05:45:59Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/src/sc/tuple.erl $").
-svn_revision("$Revision: 263 $").

-description("Functions for working with tuples.").

-testerl_export( { [], sc_tuple_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    member/2,
    sum/1

] ).





%% @spec member(E::any(), T::tuple()) -> true | false

%% @doc Checks whether E is a member element of tuple T, analogous to `lists::member(E, L)'. ```1> sc.tuple"member(b, {a,b,c}).
%% true
%%
%% 2> sc.tuple:member(d, {a,b,c}).
%% false
%%
%% 3> sc.tuple:member([1,2], {[1,2]}).
%% true'''

%% @since Version 123

member(E, T) ->

    member(E, T, 1, size(T)).



member(_E,_T, I, Sz)

    when I > Sz -> false;



member(E, T, I, Sz) ->

    case element(I, T) == E of

        true  ->
            true;

        false ->
            member(E, T, I+1, Sz)

    end.





%% @type numeric_tuple() = tuple().  Every member of a {@type numeric_tuple()} must be a {@type number()}.
%% @type relaxed_numeric_tuple() = numeric_tuple().  Relaxed numeric tuples are allowed to contain non-numeric elements, which are treated as zero for purposes of computation.

%% @spec sum(T::relaxed_numeric_tuple()) -> number()

%% @doc {@section Math} Returns the sum of the numeric elements of a tuple, treating non-numeric elements as zero. ```1>'''

%% @since Version 86

sum(T) when is_tuple(T) ->

    sum(T, 1, size(T), 0).



sum(_T, Which, Max, Work) when Which > Max ->

    Work;



sum( T, Which, Max, Work) ->

     sum(T, Which+1, Max, Work+element(Which, T)).
