%% Copyright (c) 2011 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2011-02-14 14:54:12 joe>

-module(gui1).

%% First attempt at a middle man 
%% This demonstrates dynamically sending GUI objects to a window
%% and pushing text and colored things to the browser

-compile(export_all).

start(Pid) ->
    S = self(),
    Logic = spawn_link(fun() -> logic(S) end),
    mm(Pid, Logic).

%%----------------------------------------------------------------------

logic(Pid) ->
    Pid ! clear_window,
    Pid ! {bg, "orange"},
    Pid ! {add_button, "Click me", "zapit"},
    Pid ! {add_div,"a"},
    Pid ! {add_canvas,"c"},
    Pid ! {add_random_rectangle,"c"},
    random_seed(),
    spawn(fun() -> ticker(Pid, 0) end),
    logic_loop(Pid).

ticker(Pid, N) ->
    receive
	after 250 ->
		true
	end,
    Pid ! {add_txt_to_div, "a", "Message number " ++ i2s(N)},
    Pid ! {add_random_rectangle, "c"},
    ticker(Pid, N+1).

logic_loop(Pid) ->
    receive
	Any ->
	    io:format("event loop Msg => ~p~n",[Any]),
	    logic_loop(Pid)
    end.
%%----------------------------------------------------------------------

mm(JS, Erlang) ->
    receive
	{add_canvas, Name} ->
	    JS ! {eval, add_canvas(Name)};
	{add_random_rectangle, Name} ->
	    JS ! {eval, add_random_rectangle(Name)};
	{add_div, Name} ->
	    JS ! {eval, add_div(Name)};
	{add_txt_to_div, Name, Txt} ->
	    JS ! {eval, add_txt_to_div(Name, Txt)};
	{add_button, Txt, Msg} ->
	    JS ! {eval,button(Txt,Msg)};
	clear_window ->
	    JS ! {eval,"document.body.innerHTML=''"};
	{bg, C} ->
	    JS ! {eval, ["document.body.style.backgroundColor='", C, "';"]};
	{add_text, S} ->
	    JS ! {eval, ["document.body.innerHTML +='", S, "';"]};
	X ->
	    io:format("Dropping:~p~n",[X])
    end,
    mm(JS, Erlang).

add_div(X) ->
    ["document.body.innerHTML+=\"<p><div style='border:1px solid black' id='",X,"'></div>\";"].

add_canvas(X) ->
    ["document.body.innerHTML+=\"<p><canvas width=500 height=200 id='",X,"'></canvas>\";",
     X,"= document.getElementById('",X,"').getContext('2d');"].


add_txt_to_div(X, Y) ->
    ["document.getElementById('",X,"').innerHTML = '",Y,"';"].

button(Txt, Msg) ->
    ["document.body.innerHTML+=",
     "\"<button onclick='send(\\\"",Msg,"\\\")'>",Txt,"</button>\";"].

i2s(X) ->
    integer_to_list(X).

rgb(R,G,B) ->
    ["'rgb(",i2s(R),",",i2s(G),",",i2s(B),")';"].

fillRect(X,Y,W,H) ->
    ["fillRect(",i2s(X),",",i2s(Y),",",i2s(W),",",i2s(H),")"].

add_random_rectangle(Name) ->
    %% "c.fillStyle = 'rgb(255,0,0)';c.fillRect(30, 30, 50, 50);".
    R=random:uniform(255),
    G=random:uniform(255),
    B=random:uniform(255),
    X=100+random:uniform(100),
    Y=100+random:uniform(100),
    W=random:uniform(150),
    H=random:uniform(150),
    %% "c.fillStyle = 'rgb(255,0,0)';c.fillRect(30, 30, 50, 50);".
    [Name,".fillStyle=",rgb(R,G,B),Name,".",fillRect(X,Y,W,H),";"].

random_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).

