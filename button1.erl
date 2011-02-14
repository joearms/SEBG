%% Copyright (c) 2011 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2011-02-14 09:49:23 joe>

-module(button1).

-compile(export_all).

start(Pid) ->
    random_seed(),
    put(pid, Pid), %% Ok since we'll never change this
    init(Pid),
    %% spawn(fun() -> put(pid,Pid),random_seed(), push() end),
    event_loop(Pid).

init(Pid) ->
%%     clear_page(),
%%     set_page_color("#ffffaa"),
    load_js(Pid, "svg_lib.js"),
    load_js(Pid, "jquery-1.5.min.js"),
    Pid ! {eval,"document.body.innerHTML=''"},
    Pid ! {eval, "document.body.style.backgroundColor='orange';"},
    cmd("svg = mk_canvas({width:800, height:200, color:'#ffeecc',id:1});"),
    cmd("c = document.body.appendChild(svg);"),
    cmd("button = mk_rect({});"),
    cmd("c.appendChild(button);"),
    true.
    %% cmd("logit('hello')"),

    
event_loop(Pid) ->
    receive
	Any ->
	    io:format("??event loop:~p~n",[Any]),
	    event_loop(Pid)
    end.


load_js(Pid, File) ->
    cmd(["loadScript('",File,"');"]),
    %% wait for the script to run
    receive
	{browser,_,"loaded"} ->
	    io:format("script loaded~n"),
	    true
    after 10000 ->
	    io:format("timeout loading:~p~n",[File])
    end.


cmd(Msg) ->
    io:format("eval:~s~n",[Msg]),
    get(pid) ! {eval, Msg}.
    
msg(Pid) ->
    Pid ! {send, bgColor("#ffaacc")}.

bgColor(C) ->
    ["$('body').css({'background-color':'",C,"'});"].

i2s(I) ->
    integer_to_list(I).

%% @doc Seed the random number generator.
%% This is evaluated for its side effect. Not well tested at all.

-spec random_seed() -> void.

random_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}),
    void.
