
-module(htstub_tests).
-compile(export_all).





-include_lib("eunit/include/eunit.hrl").






%http_uri:parse("https://bob:bobby@www.lunatech.com:8080/file;p=1?q=2#third"). 
%{ok,{https,"bob:bobby","www.lunatech.com",8080,"/file;p=1",
%           "?q=2#third"}}

parse_url_test_() ->

    HardUrl       = "https://bob:bobby@www.lunatech.com:8080/file;p=1?q=2#third",
    ExpectedParse = {ok,{https,"bob:bobby","www.lunatech.com",8080,"/file;p=1","?q=2#third"}},
    OurParse      = {uri,{https,"bob:bobby","www.lunatech.com",8080,"/file;p=1","?q=2#third"}},

    { "URL parsing tests", [

        { "stdlib still parses wrongly", ?_assert( ExpectedParse =:= http_uri:parse(HardUrl) ) },
        { "we parse correctly",          ?_assert( OurParse      =:= htstub:parse_url(HardUrl) ) }

    ] }.

rest_test_() ->

    { "REST router tests", [

        { "Forced fail, not implemented", ?_assert( false =:= true ) }

    ] }.