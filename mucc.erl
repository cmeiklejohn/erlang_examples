-module(mucc).

-define(SERVER, mucc).

-compile(export_all).

start() -> 
    server_util:start(?SERVER, {mucc, server_loop, [dict:new()]}).

stop() -> 
    server_util:stop(?SERVER).

register_nickname(Nickname) -> 
    global:send(?SERVER, {register, Nickname, self()}),
    receive 
        ok -> 
            ok;
        {error, Error} -> 
            Error
    end.

poll(Nickname) -> 
    global:send(?SERVER, {poll, Nickname, self()}),
    receive 
        {ok, Messages} ->
            Messages;
        Error -> 
            Error
    end.

send_message(Sender, Addressee, Message) -> 
    global:send(?SERVER, {send_message, Sender, Addressee, Message}).

server_loop(Proxies) -> 
    receive 
        {register, Nickname, Caller} -> 
            case dict:find(Nickname, Proxies) of
                error -> 
                    Pid = spawn(fun() -> proxy_client([]) end),
                    message_router:register_nickname(Nickname, Pid),
                    Caller ! ok,
                    server_loop(dict:store(Nickname, Pid, Proxies));
                {ok, _} ->
                    Caller ! {error, duplicate_nickname}, 
                    server_loop(Proxies)
            end;
        {poll, Nickname, Caller} -> 
            case dict:find(Nickname, Proxies) of
                error -> 
                    Caller ! {error, unknown_nickname};
                {ok, _} ->
                    Caller ! {get_messages, self()},
                    receive 
                        {messages, Messages} -> 
                            Caller ! {ok, Messages}
                    end
            end,
            server_loop(Proxies);
        {send_message, Sender, Addressee, Message} -> 
            case dict:find(Sender, Proxies) of
                error -> 
                    ok;
                {ok, Pid} -> 
                    Pid ! {send_message, Addressee, Message}
            end,
            server_loop(Proxies)
    end.

proxy_client(Messages) -> 
    receive 
        {print_message, Body} -> 
            proxy_client([Body|Messages]);
        {get_messages, Pid} -> 
            Pid ! {messages, lists:reverse(Messages)},
            proxy_client([]);
        {send_message, Addressee, Message} -> 
            message_router:send_message(Addressee, Message);
        stop -> 
            io:format("Proxy stopping...~n"),
            ok
    end.
