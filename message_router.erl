-module(message_router).

-define(SERVER, message_router).

-compile(export_all).

start() -> 
    server_util:start(?SERVER, {message_router, route_messages, [dict:new()]}),
    message_store:start_link().

stop() ->
    server_util:stop(),
    message_store:shutdown().

register_nickname(ClientName, Pid) -> 
    global:send(?SERVER, {register_nickname, ClientName, Pid}).

unregister_nickname(ClientName) -> 
    global:send(?SERVER, {unregister_nickname, ClientName}).

send_message(Addressee, Body) -> 
    global:send(?SERVER, {send_message, Addressee, Body}).

route_messages(Clients) -> 
    receive 
        {send_message, ClientName, Body} -> 
            case dict:find(ClientName, Clients) of
                {ok, Pid} -> 
                    Pid ! {print_message, Body};
                error -> 
                    message_store:save_message(ClientName, Body),
                    io:format("Archived message for ~p~n", [ClientName])
            end,
            route_messages(Clients);
        {register_nickname, ClientName, Pid} -> 
            Messages = message_store:find_messages(ClientName),
            lists:foreach(fun(Msg) -> Pid ! {print_message, Msg} end, Messages),
            route_messages(dict:store(ClientName, Pid, Clients));
        {unregister_nickname, ClientName} -> 
            case dict:find(ClientName, Clients) of
                {ok, Pid} -> 
                    Pid ! stop,
                    route_messages(dict:erase(ClientName, Clients));
                error -> 
                    io:format("Unknown client.~n"),
                    route_messages(Clients)
            end;
        shutdown -> 
            io:format("Shutdown! ~n");
        Oops ->
            io:format("Warning! Received: ~p~n", [Oops]),
            route_messages(Clients)
    end.
