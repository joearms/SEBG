%% Copyright (c) 2011 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2011-02-12 11:55:07 joe>

-module(raphael).

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
    load_js(Pid, "raphael-min.js"),
    load_js(Pid, "jquery-1.5.min.js"),
    Pid ! {eval,"document.body.innerHTML=''"},
    Pid ! {eval, "document.body.style.backgroundColor='orange';"},
    mk_div("canvas"),
    mk_div("log"),
    %% cmd("logit('hello')"),
    cmd("c=Raphael('canvas',800,500)"),
    cmd("r=c.circle(320,240,60);"),
    cmd("c.image('joeold.jpg', 500, 50, 294, 195);"),
    cmd("c.text(100,10,'red circle is draggble, blue box is clickable');"),
    cmd("r.attr('fill','red');"),
    cmd(drag_code()),
    cmd("r.drag(move,start,up);"),
    cmd("b=c.rect(40,40,50,50,10);"),
    cmd("b.attr('fill','blue');"),
    cmd("b.node.onclick= function(){send('click 3');};").
    

drag_code() ->
    <<"    start = function(){

	this.ox = this.attr('cx');
	this.oy = this.attr('cy');
	this.attr({opacity: 1});
    }
    
    move = function (dx, dy) {
	// move will be called with dx and dy
	this.attr({cx: this.ox + dx, cy: this.oy + dy});
    },
    
    up = function () {
	// alert('stop');
	// restoring state
	this.attr({opacity: .5});
    };
    ">>.
   


    
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

load_js(Pid, File) ->
    %% jQuery magic :-)
    cmd(["loadScript('",File,"');"]),
    %% wait for the script to run
    receive
	{browser,_,"loaded"} ->
	    io:format("script loaded~n"),
	    true
    after 10000 ->
	    io:format("timeout loading:~p~n",[File])
    end.

mkCanvas(Tag, Color, Width, Ht) ->
    cmd(["$('body').prepend('<canvas id=\"",
	Tag,"\" style=\"background:",Color,"\" width=\"",
	i2s(Width),"\" height=\"", i2s(Ht),"\"></canvas>');"]).

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
