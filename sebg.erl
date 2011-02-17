%% Copyright (c) 2011 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2011-02-17 16:25:07 joe>

-module(sebg).

%% Simple Erlang Browser Graphics

-import(lists, [map/2, reverse/1]).

-export([start/0, start/1, test/0]).

%% this has been tested run from a makefile not the shell

start() ->
    start_port(1234).

start([PortAsAtom]) ->
    Port = list_to_integer(atom_to_list(PortAsAtom)),
    start_port(Port).

start_port(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [{packet,http},
					 {reuseaddr,true},
					 {active, false}]),
    io:format("Got a listener:~p~n",[Listen]),
    spawn(fun() -> par_connect(Listen) end),
    %% wait forever. I'm not sure, but I think if the
    %% process that owns the listening socket dies we are in trouble
    receive
    after infinity ->
	    true
    end.
    
par_connect(Listen) ->
    io:format("waiting for a connection~n"),
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    spawn(fun() -> par_connect(Listen) end),
	    io:format("starting a new session socket=~p~n",[Socket]),
	    next_request(Socket);
	Other ->
	    io:format("par_connect:Other=~p ~p~n",
		      [Other,erlang:get_stacktrace()])
    end.

next_request(Socket) ->
    %% io:format("Session ~p waiting~n",[Socket]),
    Req = get_request(Socket,no,[]),
    %% io:format("req=~p~n",[element(1,Req)]),
    do_request(Socket, Req).

%%    Data is sent in the form of UTF-8 text.  Each frame of data starts
%%    with a 0xFF byte identifying the frame type, followed by the number
%%    of bytes in the data expressed as a big-endian 64 bit unsigned
%%    integer, followed by the UTF-8 data.

%%    NOTE: The length is the number of UTF-8 _bytes_, and not the number
%%    of characters.  For example, the string "Hwllo " has 13 Unicode code
%%    points, but is 21 bytes long.

do_request(Socket, {{get, {abs_path,"/connect" ++ _=Path}}, L}) ->
    io:format("~p is now a web socket~n",[Socket]),
    connect(Socket, Path, L);
do_request(Socket,{{get,{abs_path,F0}}, _}) ->
    %% io:format("Here 1F=~p~n",[F0]),
    {F, Args} = parse_uri(F0),
    File = case F of
	       "//" ++ F1 -> "/" ++ F1;
	       "/" ++ F1 -> "./" ++ F1;
	       _   ->  "./" ++ F
	   end,
    io:format("Session ~p wants:~p Args=~p~n",[Socket,File,Args]),
    Response = case file:read_file(File) of
		   {ok, Bin} ->
		       io:format("sending file:~p~n",[File]),
		       make_response(classify(File),[Bin]);
		   _ ->
		       io:format("missing file:~p Args:~p~n",[File,Args]),
		       make_response(html,pre({nosuchfile,F}))
	       end,
    gen_tcp:send(Socket, Response),
    next_request(Socket).
    
get_request(Socket,X,L) ->
    %% io:format("here X=~p L=~p~n",[X,L]),
    R  = gen_tcp:recv(Socket, 0, 30000),
    %% io:format("R=~p~n",[R]),
    case R of
	{ok, {http_request,'GET',Path,_Vsn}} ->
	    get_request(Socket,{get, Path},L);
        {ok, {http_header, _, Key, _, Val}} ->
            get_request(Socket, X, [{Key,Val}|L]);
        {ok, http_eoh} ->
            {X, L};
	_Other ->
	    io:format("session ~p ending with timeout in get_request~n",
		      [Socket]),
	    exit(timeout)
    end.

connect(Socket, Path, Headers) ->
    %% io:format("Headers=~p~n",[Headers]),
    Origin = proplists:get_value("Origin", Headers),
    Host = proplists:get_value('Host', Headers),
    Key1 = proplists:get_value("Sec-Websocket-Key1",Headers, none),
    Challenge = case Key1 of
		    none ->
			<<>>;
		    _ ->
			Key2 = proplists:get_value("Sec-Websocket-Key2", Headers),
			inet:setopts(Socket, [{packet, raw}, {active, false}]),
			Body = case gen_tcp:recv(Socket, 8, 30000) of
				   {ok, Str} -> list_to_binary(Str);
				   {error, timeout} ->
				       <<>>; 
				   _Other ->
				       <<>>
			       end,
			build_challenge(Key1, Key2, Body)
		end,
     %% prepare handhsake response
    Response = 
	["HTTP/1.1 101 WebSocket Protocol Handshake\r\n",
	 "Upgrade: WebSocket\r\n",
	 "Connection: Upgrade\r\n",
	 "Sec-WebSocket-Origin: ", Origin, "\r\n",
	 "Sec-WebSocket-Location: ws://", lists:concat([Host, Path]), "\r\n\r\n",
	 Challenge
	],
    %% io:format("Response:~p~n",[Response]),
    gen_tcp:send(Socket, Response),
    inet:setopts(Socket, [{packet, raw}, {active, true}]),
    Pid = start_session(Socket, Path),
    mm(Socket, Pid, [], 0, 0).

start_session(Socket, "/connect/" ++ ModStr) ->
    io:format("Handler in module ~p connected to socket ~p~n",[ModStr, Socket]),
    Mod = list_to_atom(ModStr),
    S = self(),
    spawn_link(fun() -> Mod:start(S) end).

mm(Socket, Pid, Buff, Rec, Sent) ->
    receive
	{tcp, Socket, Str} -> 
	    {Buff1, Rec1} = handle(Pid, Str, Buff, Rec),
	    mm(Socket, Pid, Buff1, Rec1, Sent);
	{eval, Msg} ->
	    gen_tcp:send(Socket,[0,Msg,255]),
	    %% io:format("sent:~p~n ~s~n",[Msg,Msg]),
	    mm(Socket, Pid, Buff, Rec, Sent+1);
	{tcp_closed, Socket} ->
	    exit(Pid, browser);
	{'EXIT', Pid, _} ->
	    get_tcp:closed(Socket);
	Other ->
	    io:format("unexpecte3 message dropped:~p~n",[Other]),
	    mm(Socket, Pid, Buff, Rec, Sent)
    end.

handle(Pid, [0|K], [], N) -> 
    handle(Pid, K, [], N);
handle(Pid, [255|K], Buff, N) ->  
    Pid ! {browser,N,reverse(Buff)},
    handle(Pid, K, [], N+1);
handle(Pid, [H|T], Buff, N) ->
    handle(Pid, T, [H|Buff], N);
handle(_Pid, [], Buff, N) ->
    {Buff, N}.


%% Description: Builds the challenge for a handshake response.
%% Code portions from 
%% Sergio Veiga 
%%  <http://sergioveiga.com/index.php/2010/06/17/websocket-handshake-76-in-erlang/>

build_challenge(Key1, Key2, Key3) ->
    Ikey1 = [D || D <- Key1, $0 =< D, D =< $9],
    Ikey2 = [D || D <- Key2, $0 =< D, D =< $9],
    Blank1 = length([D || D <- Key1, D =:= 32]),
    Blank2 = length([D || D <- Key2, D =:= 32]),
    Part1 = list_to_integer(Ikey1) div Blank1,
    Part2 = list_to_integer(Ikey2) div Blank2,
    Ckey = <<Part1:4/big-unsigned-integer-unit:8, 
	     Part2:4/big-unsigned-integer-unit:8, Key3/binary>>,
    erlang:md5(Ckey).

%% error(Code) ->
%%     ["HTTP/1.1 ",i2s(Code),
%%      " Error\r\nContent-Length:0\r\n\r\n"].

%% i2s(I) ->
%%     integer_to_list(I).

make_response(Tag, Data) ->
    B1  = list_to_binary(Data),
    Len = size(B1),
    Mime = mime_type(Tag),
    ["HTTP/1.1 200 Ok\r\n", content_type(Mime),
	      "Content-Length: ", integer_to_list(Len), "\r\n\r\n",
	      B1].

mime_type(gif)               -> "image/gif";
mime_type(jpg)               -> "image/jpeg";
mime_type(png)               -> "image/png";
mime_type(css)               -> "text/css";
mime_type(json)              -> "application/json";
mime_type(swf)               -> "application/x-shockwave-flash";
mime_type(html)              -> "text/html";
mime_type(xhtml)             -> "application/xhtml+xml";
mime_type(xul)               -> "application/vnd.mozilla.xul+xml";
mime_type(js)                -> "application/x-javascript";
mime_type(svg)               -> "image/svg+xml";
mime_type(X) when is_atom(X) -> mime_type(html);
mime_type(FileName)          -> mime_type(classify(FileName)).

classify(FileName) ->
    case string:to_lower(filename:extension(FileName)) of
	".gif"  -> gif;
	".jpg"  -> jpg;
	".jpeg" -> jpg;
	".css"  -> css;
	".js"   -> js;
	".svg"   -> svg;
	".xul"  -> xul;
	".html" -> html;
	".xhtml" -> xhtml;
	".htm"  -> html;
	_       -> html
    end.

content_type(X) ->
    ["Content-Type: ", X, "\r\n"].

pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T]) -> [H|quote(T)];
quote([]) -> [].

test() ->
    K1 = "3e6b263  4 17 80",
    K2 = "17  9 G`ZD9   2 2b 7X 3 /r90",
    K3 = <<"WjN}|M(6">>,
    <<"n`9eBk9z$R8pOtVb">> = build_challenge(K1, K2, K3),
    checksum_ok.

%% A typical URI looks
%% like
%% URI = "/a/b/c?password=aaa&invisible=Ahidden+value"+

parse_uri(URI) ->
    case string:tokens(URI, "?") of
	[Root] ->
	    {Root, []};
	[Root, Args] ->
	    {Root, parse_uri_args(Args)}
    end.

parse_uri_args(Args) ->
    Args1 = string:tokens(Args, "&;"),
    map(fun(KeyVal) ->
	       case string:tokens(KeyVal, "=") of
		   [Key, Val] ->
		       {urlencoded2str(Key), urlencoded2str(Val)};
		   [Key] ->
		       {urlencoded2str(Key), ""};
		   _ ->
		       io:format("Invalid str:~p~n",[KeyVal]),
		       {"error", "error"}
	       end
       end, Args1).

urlencoded2str([$%,Hi,Lo|T]) -> [decode_hex(Hi, Lo)|urlencoded2str(T)];
urlencoded2str([$+|T])       -> [$ |urlencoded2str(T)];
urlencoded2str([H|T])        -> [H|urlencoded2str(T)];
urlencoded2str([])           -> [].

%% decode_hex ...

decode_hex(Hex1, Hex2) ->
    hex2dec(Hex1)*16 + hex2dec(Hex2).

hex2dec(X) when X >=$0, X =<$9 -> X-$0;
hex2dec($A) -> 10;
hex2dec($B) -> 11;
hex2dec($C) -> 12;
hex2dec($D) -> 13;
hex2dec($E) -> 14;
hex2dec($F) -> 15;
hex2dec($a) -> 10;
hex2dec($b) -> 11;
hex2dec($c) -> 12;
hex2dec($d) -> 13;
hex2dec($e) -> 14;
hex2dec($f) -> 15.

