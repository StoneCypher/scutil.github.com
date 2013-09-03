
-module(hello_world).

-export([
    server/1
]).

server(_) -> <<"<!doctype html><html><body>Hello, world!</body></html>">>.
