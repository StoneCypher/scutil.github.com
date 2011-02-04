Stonecypher's Erlang Utility Library - reboot 2011
Written by John Haugeland, http://fullof.bs/

To skip to code, which is hundreds of lines down, search for "-module("
without quotes.

This was last tested by the author in Erl OTP 14b1 / 5.8.1.1 .  Please
run sc:test(deep) or sc:test([verbose,deep]) before using, to verify that
this library functions correectly in the current Erlang virtual machine
and environment.  Removing deep will execute a faster, less trustworthy
subset of the tests.  Removing verbose will dump much less information to
the console.

There is significant documentation.  With paths appropriate for your
system, call sc:gen_docs("/path/to/source", "/path/for/docs/to/live")
to generate.  Do not use trailing slashes.  Windows paths are fine;
remember to use \\ , because it's a string and you're quoting the
backslash.  Automatic documentation generation via edoc will then
generate HTML docs.

Past here, documentation should be generally be read in the HTML format.
