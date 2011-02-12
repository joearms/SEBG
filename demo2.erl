%% Copyright (c) 2011 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2011-02-12 10:51:26 joe>

-module(demo2).
-export([start/1]).

%% simple demo
%%   once started Pid is a channel that csn be iused to talk
%%   to the browser. Pid ! {cmd,Javascript}

start(Pid) ->
    Pid ! {eval,"document.body.innerHTML=''"},
    Pid ! {eval, "document.body.style.backgroundColor='orange';"},
    Pid ! {eval, button("click me")},
    %% now add a link
    event_loop(Pid).

button(X) ->
    ["document.body.innerHTML+=",
     "\"<button onclick='send(\\\"",X,"\\\")'>",X,"</button>\";"].

event_loop(Pid) ->
    receive
	Any ->
	    io:format("??event loop:~p~n",[Any]),
	    event_loop(Pid)
    end.
