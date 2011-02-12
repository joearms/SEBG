%% Copyright (c) 2011 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2011-02-12 11:57:32 joe>

-module(demo1).
-export([start/1]).

%% simple demo
%%   once started Pid is a channel that csn be iused to talk
%%   to the browser. Pid ! {cmd,Javascript}

start(Pid) ->
    Pid ! {eval, "document.body.innerHTML='';"},
    Pid ! {eval, "document.body.style.backgroundColor='#eeffaa';"},
    Pid ! {eval, "document.body.innerHTML+='<h1>Hello World</h1>'"},
    event_loop(Pid).

event_loop(Pid) ->
    receive
	Any ->
	    io:format("??event loop:~p~n",[Any]),
	    event_loop(Pid)
    end.

