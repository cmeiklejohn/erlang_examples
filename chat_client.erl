-module(chat_client).

-compile(export_all).

send_message(Addressee, Body) -> 
    message_router:send_message(Addressee, Body).

register_nickname(Nickname) -> 
    Pid = spawn(chat_client, handle_messages, [Nickname]), 
    message_router:register_nickname(Nickname, Pid).

handle_messages(Nickname) -> 
    receive 
        {print_message, Body} -> 
            io:format("~p received: ~p~n", [Nickname, Body]),
            handle_messages(Nickname);
        stop -> 
            ok
    end.

unregister_nickname(Nickname) -> 
    message_router:unregister_nickname(Nickname).

start_router() -> 
    message_router:start().
