-module(addbookmark).
-compile(export_all).

start(Str) ->
    %% io:format("Str=~p~n",[Str]),
    T = sebg:parse_uri_args(Str),
    %% io:format("add bookmark:~p~n",[T]),
    Time =  proplists:get_value("time", T),
    File = time_to_file(Time),
    file:write_file(File, term_to_binary(T)).

time_to_file([$\s|T]) -> [$_|time_to_file(T)];
time_to_file([$(|T])  -> [$_|time_to_file(T)]; 
time_to_file([$)|T])  -> [$_|time_to_file(T)]; 
time_to_file([H|T])   -> [H|time_to_file(T)]; 
time_to_file([])      -> ".snip".




