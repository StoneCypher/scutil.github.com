
-module(cards).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker(none).
-publicforum(none).

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Deck management for card games.").

-testerl_export( { [], cards_testsuite } ).





-export( [

    standard_suits/0,
    standard_faces/0,
    standard_backs/1

] ).




standard_suits() -> [hearts, spades, clubs, diamonds].
standard_faces() -> [ace] ++ lists:seq(2,10) ++ [jack,queen,king].

standard_backs(N) ->

    Colors = [red,blue,green,black,white,yellow,gray,orange,purple,brown,pink,teal],

    case N > length(Colors) of
        true  -> lists:seq(1,N);
        false -> {X,_} = lists:split(N, [red,blue,green,black,white,yellow,gray,orange,purple,brown,pink,teal]), X
    end.





