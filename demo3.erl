%% Copyright (c) 2011 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2011-02-12 10:55:22 joe>

-module(demo3).
-export([start/1]).

%% simple demo
%%   once started Pid is a channel that csn be iused to talk
%%   to the browser. Pid ! {cmd,Javascript}

start(Pid) ->
    Pid ! {eval,"document.body.innerHTML=''"},
    Pid ! {eval, "document.body.style.backgroundColor='grey';"},
    Pid ! {eval, button("click me")},
    %% now add a link
    event_loop(Pid).

button(X) ->
    ["document.body.innerHTML+=",
     "\"<button onclick='send(\\\"",X,"\\\")'>",X,"</button>\";"].

event_loop(Pid) ->
    receive
	{browser,_,"click me"} ->
	    Pid ! {eval, button("click me")},
	    event_loop(Pid);
	Any ->
	    io:format("??event loop:~p~n",[Any]),
	    event_loop(Pid)
    end.
