
-module(htstub_default_webserver).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
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

-description("").





-testerl_export( { [], htstub_default_webserver_testsuite } ).





-library_requirements([
    {scutil,  161},
    {testerl, 66}
]).





-export([

    verbose/3,
    diagnostic/3,
    no_site_configured_here/3

]).





%% @since Version 390

datestring() ->

    { {Y,M,D}, {H,Mn,S} } = erlang:universaltime(),

    {Hr,L} = case H of
        Hour when Hour > 12 -> {Hour-12, $p};
        Hour                -> {Hour,    $a}
    end,

    MonthLabel = case M of
        1  -> "January";
        2  -> "February";
        3  -> "March";
        4  -> "April";
        5  -> "May";
        6  -> "June";
        7  -> "July";
        8  -> "August";
        9  -> "September";
        10 -> "October";
        11 -> "November";
        12 -> "December"
    end,

    lists:flatten(io_lib:format("~s ~b ~b, ~b:~2.10.0b:~2.10.0b~c GMT",[MonthLabel,D,Y,Hr,Mn,S,L])).





%% @since Version 390

verbose(_,_,_) ->

    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"><html xml:lang=\"en-US\" lang=\"en-us\"><head><title>The HtStub Application Webserver</title><style type=\"text/css\">body{background-color:#def;margin:0;padding:4em 3em 2em 3em;border:0;}#topbar{position:fixed;top:0;left:0;right:0;height:1.5em;border-bottom:1px solid black;background-color:#333;vertical-align:middle;color:#35f;}#topinner{margin:0 0.5em;}#topbar a{font-size:80%;color:#8af;text-decoration:none;border-bottom:1px solid #36a;margin:0 0.25em;padding:0;}#topbar a:hover{color:yellow;border-bottom-color:#884;}#topbarright{float:right;margin:0 0.5em 0 0;height:100%;}h1,h2,p,dt,dd,li,#topinner,a{font-family:candara,jensen,palatino,sans-serif;text-align:justify;line-height:135%;}html,h1,h2,p{margin:0;padding:0;}h1,h2{border-bottom:1px solid #888;}h1+p,h2+p{margin-top:1.5em;}h2{margin-top:2.5em;}p{text-indent:2.75em;}p+p{margin-top:1.1em;}#rightbox{float:right;width:25em;background-color:#cde;margin:0 0 3em 3em;}#johnhaugeland,#scutil{margin:1em;padding:1em;background-color:#bcd;}#johnhaugeland h2,#scutil h2{margin:0;padding:0;}#johnhaugeland ul,#scutil ul{margin:0;padding:0;list-style-type:none;line-height:100%;}#johnhaugeland li,#scutil li{margin:0.75em 0 0 0;padding:0.1em 0.2em;font-size:90%;background-color:#abc;line-height:100%;}#johnhaugeland a,#johnhaugeland a:visited,#scutil a,#scutil a:visited{color:#008;}#twitter_div{display:none;}dl{margin:1em 2em;background-color:#f0f8ff;padding:1em 2em;border:1px solid #bcd;}dt{font-weight:bold;}dd{font-size:90%;margin-left:1.5em;padding-left:0;}dd+dt{margin-top:1em;}#mainbox{margin-right:28em;}code{font-weight:bold;font-family:consolas,\"lucida console\",\"courier new\",monospace;font-size:100%;}#mainbox div.codediv{background-color:#dfe;padding:0.25em 0.5em;margin:1em 3em 0 3em;font-size:100%;}</style><script type=\"text/javascript\" language=\"Javascript\">function WrapJH(RealArg, Tgt) {twitterCallback2(RealArg);document.getElementById('johnhaugelanddata').innerHTML = document.getElementById('twitter_update_list').innerHTML;}function WrapSU(RealArg, Tgt) {twitterCallback2(RealArg);document.getElementById('scutildata').innerHTML = document.getElementById('twitter_update_list').innerHTML;}</script></head><body><div id=\"topbar\"><span id=\"topbarright\"><a href=\"http://fullof.bs/\">FullOf.BS</a> &bull; <a href=\"http://crunchyd.com/\">Crunchy Development</a> &bull; <a href=\"http://WhyIHateTheGPL.com/\">WhyIHateTheGPL.com</a> &bull; <a href=\"http://guaranteedvps.com/\" style=\"color:#0d0;font-weight:bold;\">Guaranteed VPS</a></span><div id=\"topinner\"><a href=\"http://htstub.com/\">HtStub</a> &bull; <a href=\"http://scutil.com/\">ScUtil</a> &bull; <a href=\"http://htstub.com/\">Documentation</a> &bull; <a href=\"http://scutil.com/\">Examples</a></div></div><div id=\"rightbox\"><div id=\"scutil\"><h2>ScUtil Update Twitter</h2><ul id=\"scutildata\"></ul><a href=\"http://twitter.com/JohnHaugeland\" id=\"twitter-link\" style=\"display:block;text-align:right;\">follow ScUtil on Twitter</a></div><div id=\"johnhaugeland\"><h2>John Haugeland's Twitter</h2><ul id=\"johnhaugelanddata\"></ul><a href=\"http://twitter.com/JohnHaugeland\" id=\"twitter-link\" style=\"display:block;text-align:right;\">follow John Haugeland on Twitter</a></div><div id=\"twitter_div\"><h2 class=\"sidebar-title\">Twitter Updates</h2><ul id=\"twitter_update_list\"></ul><a href=\"http://twitter.com/JohnHaugeland\" id=\"twitter-link\" style=\"display:block;text-align:right;\">follow me on Twitter</a></div><div id=\"ohloh_div\"></div></div><div id=\"mainbox\"><h1>The HtStub Application Webserver</h1><p>The <a href=\"http://htstub.com/\">HtStub Application Webserver</a> is installed, running, and listening on this port. This port was started with the HtStub default webserver function.</p><p>HtStub is a part of <a href=\"http://scutil.com/\">the ScUtil library</a> by <a href=\"http://fullof.bs/\">John Haugeland</a>, and is MIT licensed.</p><h2>Why am I seeing this?</h2><p>This site's HtStub webserver was started with the generic configuration, meaning that it isn't currently running an application. <a href=\"#todo\">This tutorial</a> shows an enterprising developer how to get started. Generally, this page is used as a test, to make sure a new server is behaving as expected.</p><p>Want to be up and running instantly?  Try this:</p><div class=\"codediv\"><code>htstub:listen([ <span style=\"color:#600\">{port,9000}</span>, <span style=\"color:#008\">{handler, fun(_) -> <span style=\"color:#408\">\"&lt;html&gt;&lt;body&gt;Hello, world!&lt;/body&gt;&lt;/html&gt;\"</span> end}</span> ]).</code></div><h2>What is HtStub?</h2><p>HtStub is an application oriented webserver for the <a href=\"http://erlang.org/\">Erlang</a> programming language. HtStub is extremely high performance and extremely low configuration; in most cases, HtStub webservers are single functions and relatively short. HtStub has very low memory requirements, can support massive parallelism, and is well suited to handling a wide variety of tasks:</p><dl><dt>Application Webservers</dt><dd>The main purpose of HtStub is developing new web applications. HtStub is built in The Unix Way, meaning intended to be a small piece to be interlocked with other systems. HtStub's approach is to provide the bare minimum to be conceptually a webserver: it handles the sockets, it parses incoming HTTP requests, it calls a provided handler when something happens, and it dispatches the result of said handler to the client once appropriate. Other than that, it gets out of the way; HtStub has no concept of filesystem access, scripting languages, application interfaces, or indeed anything outside what's needed to support HTTP. HtStub is a pure application webserver: it provides the web serving part and hands over all logic to its handler. This allows developers to create web applications that are blisteringly fast without any glue, any configuration, any setup or any hassle; just pick a port, pass a function, maybe set some options in a tuple, and it's ready to go.</dd><dt>Infrastructure for web platforms</dt><dd>Not every wheel needs reinvention. An author who wants to build a web framework without building the webserver backend infrastructure will find HtStub a simple and straightforward way to work. HtStub's modular chaining interface makes writing web filters in the fashion of Unix pipe filters straightforward.</dd><dt>Generated webservers</dt><dd>Not every wheel needs reinvention. Many web frameworks have been tied to a specific webserver technology, in the way that ErlyWeb was built on YAWS; other web frameworks (eg Nitrogen) are built to run on top of many different webservers. HtStub's minimal nature and function-based interface</dd><dt>Adding web interfaces to existing Erlang applications</dt><dd>Another major secondary use of HtStub is to add web functionality to an existing application. Because all it takes is a single function, it's generally simple to write an initial web interface as a simple transformation for existing console or pipe reporting behavior, then to expound therefrom. Reducing barriers and time to entry makes it easier to justify modernizing an application interface.</dd><dt>Serving static or near-static content</dt><dd>One of the major secondary uses of HtStub is to serve static and near-static content. A natural, highly efficient way to represent content is as pre-fetched (or pre-assembled) binaries in memory, removing the disk bottleneck in a form similar to memcached, but with immediate absolute certainty what correct content is, leading to higher performance.</dd><dt>Reporting and testing interfaces</dt><dd>Because of the trivial nature of providing a web service through HtStub, it is a natural choice for people engaging in test driven development, who'd like readable, easily network invokable tests ready in a matter of seconds to enable testing prior to development. This is further enhanced by an integration for TestErl, the testing library which is part of the same master library (ScUtil) that HtStub is.</dd><dt>Comet, Reverse-AJAX, BOSH and Bayeux servers</dt><dd>Because of Erlang's parallelism mechanisms, nearly any well designed Erlang webserver will be well suited to mechanisms such as Comet, where the server's response to a meaningless query is postponed indefinately, with the understanding that when the server wishes to simulate a push event, it will respond to the generic hanging event. HtStub's minimalist design is especially well suited to scaling this style of server significantly.</dd></dl><h2>Why another webserver?</h2><p>There are lots of great webservers out there that fulfill both broad capacities and narrow niches. We have great respect for projects like Apache, Nginx, Lightstreamer, Lighttpd, Yaws, Mochiweb and so forth. However, we want something that works somewhat differently.</p><p>HtStub is meant as a library to ease the use of the Erlang platform for the development of large web services, meant for people who don't actually want large libraries and infrastructures. This library is geared for speed, flexibility, minimalism and low impact. Whereas the applications and libraries that exist already do a wonderful job, HtStub seeks to fill certain specific niches which are not yet well serviced, but which are well suited to the inherent compiled long-running cross-machine parallelism that the Erlang platform can bring to bear.</p><p>Furthermore, because the author <a href=\"http://WhyIHateTheGPL.com/\">feels strongly against the GPL</a>, HtStub is MIT-licensed, meaning that it is fully available for commercial endeavors. The author would appreciate backlinks and code donations as user entities saw fit, of course.</p><h2>How can I help?</h2><p>Spread the word! ScUtil covers a bunch of languages: C++, PHP, Erlang, JavaScript, ActionScript, MXML, Delphi, XML, CSS, Prolog and more. You know <em>someone</em> that needs it.</p></div><script type=\"text/javascript\" src=\"http://twitter.com/javascripts/blogger.js\"></script><script type=\"text/javascript\" src=\"http://twitter.com/statuses/user_timeline/ScUtil.json?callback=WrapSU&amp;count=8\"></script><script type=\"text/javascript\" src=\"http://twitter.com/statuses/user_timeline/JohnHaugeland.json?callback=WrapJH&amp;count=8\"></script><script type=\"text/javascript\">var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));</script><script type=\"text/javascript\">try {var pageTracker = _gat._getTracker(\"UA-8996499-2\");pageTracker._trackPageview();} catch(err) {}</script><p style=\"position:absolute; right:0;\">Generated " ++ datestring() ++ "</p></body></html>".





%% @since Version 390

diagnostic(Request, Headers, Arguments) ->

    {Site, Port, { http, Major, Minor, Method }} = Request,
    {Resource, GetArgs, {_Length, Body}}         = Arguments,

    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"><html xml:lang=\"en-US\" lang=\"en-us\"><style type=\"text/css\">body { font-family: candara, sans-serif; margin: 0; padding: 0 2em; } dl { margin: 0 2em; } table { margin: 0 2em; border: 2px solid black; border-collapse: collapse; } td { border: 1px solid #666; padding: 0.1em 0.3em; text-align: right; background-color: #def; } td+td { text-align: justify; background-color: #eee; } dd { padding: 0.25em 0.5em; background-color: #eee; } tt { display: block; background-color: #eee; padding: 0.25em 0.5em; margin: 0 2em; }</style><head><title>Diagnostics</title></head><body><h1>Request</h1><dl><dt>Site</dt><dd>" ++
    Site ++

    "</dd><dt>Port</dt><dd>" ++
    integer_to_list(Port) ++

    "</dd><dt>Protocol</dt><dd>http, " ++
    integer_to_list(Major) ++ ", " ++ integer_to_list(Minor) ++ " " ++ htstub:html_encode(Method) ++

    "</dd><dt>Resource</dt><dd>" ++
    htstub:html_encode(Resource) ++

    "</dl><h1>Headers</h1><table>" ++
    lists:flatten([ "<tr><td>" ++ htstub:html_encode(Header) ++ "</td><td>" ++ htstub:html_encode(Val) ++ "</td></tr>" || {Header,Val} <- Headers ]) ++

    "</table><h1>Arguments</h1><table>" ++
    lists:flatten([ "<tr><td>" ++ htstub:html_encode(K) ++ "</td><td>" ++ htstub:html_encode(V) ++ "</td></tr>" || {K,V} <- GetArgs ]) ++

    "</table><h1>Body</h1><tt>" ++
    htstub:html_encode(Body) ++

    "</tt></body></html>".





%% @since Version 390

no_site_configured_here(_,_,_) ->

    { 404, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"><html xml:lang=\"en-US\" lang=\"en-us\"><head><title>No page configured here</title><style type=\"text/css\" media=\"screen,projection,pring\">body{background-color:#def;margin:0;padding:2em 3em;border:0;}h1,a{font-family:candara,jensen,palatino,sans-serif;text-align:justify;line-height:135%;}h1{margin:0;padding:0;}#corner{text-align:right;position:absolute;right:3em;bottom:2em;}a{color:#008;text-decoration:none;border-bottom:1px solid #06c;font-weight:bold;padding:0;line-height:100%;}.date{font-size:75%;font-family:constantia,\"times new roman\",serif;}</style></head><body><h1>There is no website here</h1><p id=\"corner\"><a href=\"http://htstub.com/\" title=\"The HtStub Erlang Webserver\">HtStub</a><br/><span class=\"date\">Served " ++ datestring() ++ "</span></p></body></html>" }.
