
-module(sc_cq).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker("http://crunchyd.com/forum/project.php?projectid=7").
-publicforum("http://crunchyd.com/forum/scutil-discussion/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Efficient circular queue.").

-testerl_export( { [], scutil_testsuite } ).
-library_requirements( [ {dq,141}, {testerl,16} ] ).





-export( [

    create/1,
%      create/2,

%    write/2,

%    read/1,

%    peek/1,
%      peek/2

] ).




create(Size) ->

    list_to_tuple( [sc_cq, 1, list_to_tuple( lists:duplicate(Size,0) )] ).
