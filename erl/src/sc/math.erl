
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





-module(sc.math).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Math routines and facilities").

-testerl_export( { [], sc_math_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    mersenne_prime/1,            % needs tests
    floor/1,                     % needs tests
    ceil/1,                      % needs tests
    ceiling/1,                   % needs tests
    factorial/1,                 % needs tests
    square/1,                    % needs tests
    cube/1,                      % needs tests
    mod/2,                       % needs tests
    root_sum_square/1,           % needs tests
    root_mean_square/1,          % needs tests
    absolute_difference/2        % needs tests

] ).





%% @since Version 201

mersenne_prime(Which) -> mersenne_prime_worker(Which, 1).



%% @private

mersenne_prime_worker(0, Current) ->

    Current - 1;



mersenne_prime_worker(Remain, Current) when Remain > 30 ->

    mersenne_prime_worker(Remain-30, Current*1073741824);



mersenne_prime_worker(Remain, Current) ->

    mersenne_prime_worker(Remain-1, Current*2).





%% @since Version 172

floor(X) ->

     floor_t(trunc(X), trunc(X)-X).



%% @private

floor_t(T, Td) when Td < 0 -> T;
floor_t(T, Td) when Td > 0 -> T-1;
floor_t(T,_Td)             -> T.





%% @since Version 172
%% @equiv ceiling(X)

ceil(X) -> 

     ceiling(X).





%% @since Version 172

ceiling(X) ->

     ceiling_t(trunc(X), trunc(X)-X).



%% @private

ceiling_t(T, Td) when Td < 0 -> T+1;
ceiling_t(T, Td) when Td > 0 -> T;
ceiling_t(T,_Td)             -> T.





%% @since Version 168

factorial(X) ->

    factorial(X, 1).



%% @private

factorial(0, _Counter) -> 

    0;
    
    

factorial(1, Counter) -> 

    Counter;



factorial(X, Counter) when is_integer(X), X > 1 ->

    factorial(X-1, Counter*X).





%% @spec square(Input::number()) -> number()

%% @doc {@section Math} Squares the input; convenient in list comprehensions to prevent recalculation, and clear in the fashion of documentary functions. ```1> scutil:square(2).
%% 4
%%
%% 2> scutil:square(2.5).
%% 6.25'''

%% @since Version 108

square(X) -> 
    
    X*X.





%% @spec cube(Input::number()) -> number()

%% @doc {@section Math} Cubes the input; convenient in list comprehensions to prevent recalculation, and clear in the fashion of documentary functions. ```1> scutil:cube(2).
%% 8
%%
%% 2> scutil:cube(2.5).
%% 6.25'''

%% @since Version 165

cube(X) -> 

    X*X*X.





%% @spec mod(Base::integer(), Range::integer()) -> integer()

%% @doc {@section Math} Takes the modulus of an integer by another integer.  Luckily, erlang calls what most languages refer to as modulus by its correct name, remainder (c's `%', erlang's `rem').  Modulus is implemented incorrectly in nearly every language, because chip vendors implement remainder and the wrong name stuck.  The difference is in how the operator reacts to a negative `Base': -10 modulo 3 is 2, whereas -10 rem 3 is -1.  Remainder takes the residue of dividing the base by the lowest (nearest negative infinity) integer N adjacent the real valued divisor; modulo returns the highest, which is less CPU efficient but always provides an answer on [0..Range-1]. ```1> scutil:mod(10,3).
%% 1
%%
%% 2> [ scutil:mod(X,4) || X <- lists:seq(-10,10) ].
%% [2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2]'''
%%
%% @since Version 29

mod(Base, Range) when is_integer(Base), is_integer(Range) ->

    case Base rem Range of

        X when X < 0 ->
            X + Range;

        Z ->
            Z

    end.

%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <p></p>


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







%% @spec root_sum_square(VX::vector()) -> number()

%% @doc {@section Math} Calculate the magnitude (also known as the root sum square)

%% @since Version 85

root_sum_square(VX) when is_list(VX) ->
    
    math:sqrt(lists:sum([ X*X || X <- VX ]));



root_sum_square(VX) when is_tuple(VX) -> 

    root_sum_square(tuple_to_list(VX)).





%% @spec root_mean_square(Values::numericlist()) -> float()

%% @doc {@section Statistics} Calculates the root mean square of the values in the list.  ```1> scutil:root_mean_square([1,2,3,4,5]).
%% 3.3166247903554
%%
%% 2> scutil:root_mean_square([2,2,2]).
%% 2.0'''

%% @since Version 39

root_mean_square(List) when is_list(List) ->

    math:sqrt(.sc.stats.means:arithmetic([ Val*Val || Val <- List ])).





%% @spec absolute_difference(A::number(), B::number()) -> number()

%% @doc {@section Documentary} Takes the absolute value of the difference between the two arguments.  Offered mostly to make dependant code clearer. ```1> scutil:absolute_difference(1.25, 1).
%% 0.25'''

%% @since Version 39

absolute_difference(A,B) -> 

    abs(A-B).
