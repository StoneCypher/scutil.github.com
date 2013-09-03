
%% @doc test

-module(restaurant).





-export([

    server/1

]).





restaurants() ->

    [
        <<"Tony Baloneys">>,
        <<"Tartine">>,
        <<"Foreign Cinema">>,
        <<"Super Duper">>,
        <<"Roam">>,
        <<"Espetus">>,
        <<"SoMa StrEat Food">>,
        <<"Pizza Hut">>
    ].





server(_) -> 

    << 
        <<"<!doctype html><html><body>">> / binary, 
        (sc:random_from( restaurants() )) / binary, 
        <<"</body></html>">>              / binary
    >>.
