
-module(htstub).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://htstub.com/").
-twitter({"JohnHaugeland", "http://twitter.com/JohnHaugeland"}).
-twitter({"ScUtil", "http://twitter.com/ScUtil"}).
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker(none).
-publicforum(none).
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").
-svn_date("$Date$").

-description("The HtStub application webserver API").

-todo([unit_tests,stochastic_tests,lib_test_rig_integrate,dialyzer,docs,doc_examples,doc_extraction,build_system]).





-testerl_export( { [], htstub_testsuite } ).





-library_requirements([
    {scutil,  161},
    {testerl, 66}
]).





-export([

    serve/0,
      serve/1,
      serve/2,

    url_decode/1,

    html_encode/1,
    html_decode/1

]).




default_options() ->

    [   { port,    80                                     } ,
        { ip,      { inet, {0,0,0,0} }                    } ,
        { timeout, 300000                                 } ,
        { handler, fun htstub_default_webserver:verbose/3 }
    ].





serve() ->

    serve([]).



serve(Handler) when is_function(Handler) ->

    serve([{handler,Handler}]);



serve(Options) ->

    { htstub_server, spawn(htstub_core, create_server, [lists:keymerge(1, Options, default_options())] ) }.



serve(Port,Handler) when is_integer(Port), is_function(Handler) ->

    serve([{port,Port},{handler,Handler}]).





url_decode(Url) ->

    url_decode(Url, []).



url_decode([], Work) ->

    lists:reverse(Work);



url_decode([$%, A, B | Rem], Work) ->

    url_decode(Rem, [erlang:list_to_integer([A,B],16)] ++ Work);



url_decode([A | Rem], Work) ->

    url_decode(Rem, [A] ++ Work).





html_encode(String) ->

    html_encode(String, []).

    
    
html_encode([], Work) ->

    lists:flatten(lists:reverse(Work));



html_encode([ $" | Rem ], Work) ->                                                                                        %"% Bad highlighter

    html_encode(Rem, ["&quot;"]++Work);



html_encode([ $& | Rem ], Work) ->

    html_encode(Rem, ["&amp;"]++Work);



html_encode([ $< | Rem ], Work) ->

    html_encode(Rem, ["&lt;"]++Work);



html_encode([ $> | Rem ], Work) ->

    html_encode(Rem, ["&gt;"]++Work);



html_encode([ Normal | Rem ], Work) ->

    html_encode(Rem, [Normal]++Work).





html_decode(_) -> todo.
