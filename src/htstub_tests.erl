
-module(htstub_tests).
-compile(export_all).





-include_lib("eunit/include/eunit.hrl").
-include("htstub.hrl").





%http_uri:parse("https://bob:bobby@www.lunatech.com:8080/file;p=1?q=2#third"). 
%{ok,{https,"bob:bobby","www.lunatech.com",8080,"/file;p=1",
%           "?q=2#third"}}

%% see http://blog.lunatech.com/2009/02/03/what-every-web-developer-must-know-about-url-encoding
%% see http://doriantaylor.com/policy/http-url-path-parameter-syntax

expected_hard_parse() ->

    #htstub_uri{ 
        scheme       = https, 
        user         = <<"bob">>, 
        password     = <<"bobby">>, 
        host         = <<"www.lunatech.com">>, 
        port         = 8080, 
        path         = <<"/file">>,
        path_params  = [{<<"p">>,[<<"1">>]}],
        query_params = [{<<"q">>,[<<"2">>]}],
        fragment     = <<"third">>
     }.





expected_harder_parse() -> 
    
    #htstub_uri{ 
        scheme       = https, 
        user         = <<"bob">>, 
        password     = <<"bobby">>, 
        host         = <<"www.lunatech.com">>, 
        port         = 8080, 
        path         = <<"/file">>,
        path_params  = [{"p",["1","2","3"]},{"q",["1"]},{"r"},{"saa"},{"taa",["1","2",""]},{"u"}],
        query_params = [{"r"},{"q",["2,3","4"]},{"waa",["1,2"]}],
        fragment     = "third"
     }.





expected_easy_parse() -> 
    
    #htstub_uri{ 
        scheme       = http, 
        user         = undefined, 
        password     = undefined, 
        host         = <<"foo.com">>, 
        port         = 80, 
        path         = <<"/">>,
        path_params  = [],
        query_params = [],
        fragment     = undefined
     }.





parse_url_test_() ->

    HardUrl       = "https://bob:bobby@www.lunatech.com:8080/file;p=1?q=2#third",
    ExpectedParse = { ok, {https,"bob:bobby","www.lunatech.com",8080,"/file;p=1","?q=2#third"} },

    HarderUrl     = "https://bob:bobby@www.lunatech.com:8080/file;p=1,2,3;q=1;r;saa;taa=1,2,;u?q=2,3;r;waa=1,2;q=4#third",
    SimpleUrl     = "http://foo.com/",

    { "URL parsing tests", [

        { "stdlib still parses wrongly",   ?_assert( ExpectedParse           =:= http_uri:parse(HardUrl)     ) },
        { "we parse correctly",            ?_assert( expected_hard_parse()   =:= htstub:parse_uri(HardUrl)   ) },
        { "we parse harder correctly too", ?_assert( expected_harder_parse() =:= htstub:parse_uri(HarderUrl) ) },
        { "we parse simple correctly",     ?_assert( expected_easy_parse()   =:= htstub:parse_uri(SimpleUrl) ) }

    ] }.





rest_test_() ->

    { "REST router tests", [

%       { "Forced fail, not implemented", ?_assert( false =:= true ) }

    ] }.