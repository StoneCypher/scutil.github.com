
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

    mameluke_deck/0,
    french_deck/0,
    american_deck/0,
    german_deck/0,
    domino_deck/0,
    chinese_money_deck/0,
    piquet_deck/0,
    pinochle_deck/0,
    tarot_deck/0,
    doppelkopf_deck/0, doppelkopf_deck/1,
    euchre_deck/0,
    hungarian_deck/0,
    austrian_deck/0,
    mille_bornes_deck/0,
    swiss_deck/0,
    italian_deck/0,
    uno_deck/0

] ).





%% @spec mameluke_deck() -> list()
%% @doc One of the oldest decks of cards known, originating from Egypt; probably the point between Chinese Money Cards and the early European decks. 
%% ```1> scutil:columnate(cards:mameluke_deck(), [{columns,4}]).
%% ["   {1,polo_stick}         {1,coin}         {1,sword}         {1,cup}   ",
%%  "   {2,polo_stick}         {2,coin}         {2,sword}         {2,cup}   ",
%%  "   {3,polo_stick}         {3,coin}         {3,sword}         {3,cup}   ",
%%  "   {4,polo_stick}         {4,coin}         {4,sword}         {4,cup}   ",
%%  "   {5,polo_stick}         {5,coin}         {5,sword}         {5,cup}   ",
%%  "   {6,polo_stick}         {6,coin}         {6,sword}         {6,cup}   ",
%%  "   {7,polo_stick}         {7,coin}         {7,sword}         {7,cup}   ",
%%  "   {8,polo_stick}         {8,coin}         {8,sword}         {8,cup}   ",
%%  "   {9,polo_stick}         {9,coin}         {9,sword}         {9,cup}   ",
%%  "  {10,polo_stick}        {10,coin}        {10,sword}        {10,cup}   ",
%%  " {duke,polo_stick}      {duke,coin}      {duke,sword}      {duke,cup}  ",
%%  "{viceroy,polo_stick}   {viceroy,coin}   {viceroy,sword}   {viceroy,cup}",
%%  " {king,polo_stick}      {king,coin}      {king,sword}      {king,cup}  "]'''
%% @since Version 200
mameluke_deck()      -> [ {Face,Suit} || Face <- [1,2,3,4,5,6,7,8,9,10,duke,viceroy,king], Suit <- [polo_stick,coin,sword,cup] ].

%% @since Version 200
french_deck()        -> [ {Face,Suit} || Face <- [ace,2,3,4,5,6,7,8,9,10,knave,queen,king], Suit <- [hearts,spades,diamonds,clubs] ].

%% @since Version 200
american_deck()      -> [ {Face,Suit} || Face <- [ace,2,3,4,5,6,7,8,9,10,jack,queen,king], Suit <- [hearts,spades,diamonds,clubs] ] ++ [ {joker,big}, {joker,little} ].

%% @since Version 200
german_deck()        -> [ {Face,Suit} || Face <- [ace,2,3,4,5,6,7,8,9,10,jack,queen,king], Suit <- [hearts,bells,leaves,acorns] ] ++ [ {joker,big}, {joker,little} ].

%% @since Version 200
domino_deck()        -> [ {A,B} || A <- [1,2,3,4,5,6], B <- [1,2,3,4,5,6], A =< B ].

%% @since Version 200
chinese_money_deck() -> [ {Val,Suit} || Val <- [2,3,4,5,6,7,8,9], Suit <- [coins,strings,myriads,tens_of_myriads] ] ++ [{1,tens_of_myriads}].   % that's right, 2-9 in each suit except tens, which is 1-9.

%% @since Version 200
piquet_deck()        -> [ {Face,Suit} || Face <- [ace,7,8,9,10,jack,queen,king], Suit <- [hearts,spades,diamonds,clubs] ].

%% @since Version 200
pinochle_deck()      -> [ {Face,Suit} || Face <- [ace,9,10,jack,queen,king], Suit <- [hearts,spades,diamonds,clubs] ].

%% @since Version 200
tarot_deck()         -> [ {Face,Suit} || Face <- [ace,2,3,4,5,6,7,8,9,10,jack,knight,queen,king], Suit <- [hearts,spades,diamonds,clubs] ] ++ [ {major,X} || X <- lists:seq(1,21) ] ++ [{the_fool,0}].

%% @since Version 200
doppelkopf_deck()         -> doppelkopf_deck(nines).

%% @since Version 200
doppelkopf_deck(nines)    -> [ {Face,Suit} || Face <- [ace,9,10,jack,queen,king], Suit <- [hearts,bells,leaves,acorns] ];
doppelkopf_deck(no_nines) -> [ {Face,Suit} || Face <- [ace,10,jack,queen,king], Suit <- [hearts,bells,leaves,acorns] ].

%% @since Version 200
hungarian_deck() -> [ {Face,Suit} || Face <-    [vii,viii,ix,x,under,over,king,ace], Suit <- [hearts,bells,leaves,acorns] ].

%% @since Version 200
austrian_deck()  -> [ {Face,Suit} || Face <- [vi,vii,viii,ix,x,under,over,king,ace], Suit <- [hearts,bells,leaves,acorns] ].

%% @since Version 200
swiss_deck()     -> [ {Face,Suit} || Face <- [6,7,8,9,banner,under,over,king,ace], Suit <- [roses,bells,acorns,shields] ].

%% @since Version 200
italian_deck()   -> [ {Face,Suit} || Face <- [ace,2,3,4,5,6,7,infantry,cavalry,king], Suit <- [coins,swords,cups,clubs] ].

%% @since Version 200
euchre_deck()    -> [ {Face,Suit} || Face <- [9,10,jack,queen,king,ace], Suit <- [hearts,spades,diamonds,clubs] ].

%% @since Version 200
mille_bornes_deck() -> scutil:key_duplicate([
  {3,{hazard,accident}},{3,{hazard,out_of_gas}},{3,{hazard,flat_tire}},{4,{hazard,speed_limit}},{5,{hazard,stop}},
  {6,{remedy,repair}},{6,{remedy,gasoline}},{6,{remedy,spare_tire}},{6,{remedy,end_of_speed_limit}},{14,{remedy,go}},
  {1,{safety,driving_ace}},{1,{safety,extra_tank}},{1,{safety,puncture_proof}},{1,{safety,right_of_way}},
  {10,{distance,25}},{10,{distance,50}},{10,{distance,75}},{12,{distance,100}},{4,{distance,200}}
]).

%% @since Version 200
uno_deck() -> [ {Face,Color} || Face <- lists:seq(0,9)++lists:seq(1,9)++[reverse,reverse,skip,skip,draw_two,draw_two], Color <- [red,yellow,green,blue] ] ++ lists:flatten(lists:duplicate(4, [{draw_four,wild},{wild,wild}])).
