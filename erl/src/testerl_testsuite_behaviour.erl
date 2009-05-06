
-module(testerl_testsuite_behaviour).
-vsn(0).
-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://testerl.com/modules/testerl_suite/").
-publicsvn("").

-testerl(testerl_suite_testsuite).

-description("Defines the behavior used in implementing a testerl suite.").





-export([behaviour_info/1]).





behaviour_info(callbacks)         -> [{run,2}];
behaviour_info(_UnknownParameter) -> undefined.
