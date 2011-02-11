%% Copyright (c) 2011 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2011-02-11 18:12:19 joe>

-module(svg_demo).

-compile(export_all).

start(Pid) ->
    elib1_misc:random_seed(),
    put(pid, Pid), %% Ok since we'll never change this
    init(),
    spawn(fun() -> put(pid,Pid),random_seed(), push() end),
    event_loop(Pid).

init() ->
    clear_page(),
    set_page_color("#ffffaa"),
    load_js("raphael-min.js"),
    mk_div("canvas"),
    mk_div("log"),
    cmd("logit('hello')"),
    cmd("glob[1]=Raphael('canvas',800,500)"),
    cmd("glob[1].image('joeold.jpg', 100, 100, 200, 300);"),
    cmd("glob[1].text(100,10,'red circle is draggble, blue box is clickable');"),
    cmd("glob[2]=glob[1].circle(320,240,60);"),
    cmd("glob[2].attr('fill','red');"),
    cmd("glob[2].drag(move,start,up);"),
    cmd("glob[3]=glob[1].rect(40,40,50,50,10);"),
    cmd("glob[3].attr('fill','blue');"),
    cmd("glob[3].node.onclick= function(){send('click 3');};").

event_loop(Pid) ->
    receive
	Any ->
	    io:format("??event loop:~p~n",[Any]),
	    event_loop(Pid)
    end.

push() ->
    %% every 100 ms push a random rectangle to the browser
    sleep(100),
    cmd(random_rect()),
    push().

random_rect() ->
    R=random:uniform(255),
    G=random:uniform(255),
    B=random:uniform(255),
    X=400+random:uniform(100),
    Y=50+random:uniform(100),
    W=random:uniform(250),
    H=random:uniform(100),
    ["glob[1].rect(",i2s(X),",",i2s(Y),",",i2s(W),",",i2s(H),").",
     "attr('fill',", rgb(R,G,B),");"].
    
rgb(R,G,B) ->
    ["'rgb(",i2s(R),",",i2s(G),",",i2s(B),")'"].

sleep(N) ->
    receive
	after
	    N ->
		true
	end.

clear_page() ->
    cmd("$('body').html('');").

set_page_color(C) ->
    cmd(["$('body')",css([{"background-color",C}]),";"]).

css(L) ->
    [".css({", [[fmt(I),":",fmt(J)]||{I,J} <- L],"})"].

fmt(I) ->
    ["'",I,"'"].

mk_div(Id) ->
    cmd(["$('body').prepend('<div id=\"",Id,"\"></div>');"]).

load_js(File) ->
    %% jQuery magic :-)
    cmd(["$.getScript('./",File,"',function(){send('ack');});"]),
    %% wait for the script to run
    receive
	{browser,_,"ack"} ->
	    true
    after 10000 ->
	    io:format("timeout loading:~p~n",[File])
    end.

mkCanvas(Tag, Color, Width, Ht) ->
    cmd(["$('body').prepend('<canvas id=\"",
	Tag,"\" style=\"background:",Color,"\" width=\"",
	i2s(Width),"\" height=\"", i2s(Ht),"\"></canvas>');"]).

cmd(Msg) ->
    get(pid) ! {send, Msg}.
    
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
