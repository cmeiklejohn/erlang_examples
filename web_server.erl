-module(web_server).

-define(OK, <<"ok">>).

-export([start/1, stop/0, dispatch_requests/1]).

start(Port) -> 
    mochiweb_http:start([{port, Port}, {loop, fun dispatch_requests/1}]).

stop() -> 
    mochiweb_http:stop().

dispatch_requests(Req) -> 
    Path = Req:get(path),
    Action = clean_path(Path),
    handle(Action, Req).

handle(Action, Req) -> 
    Params = Req:parse_qs(), 
    case Action of 
        "/send" -> 
            Sender = proplists:get_value("nickname", Params),
            Addressee = proplists:get_value("to", Params), 
            Message = proplists:get_value("msg", Params),
            mucc:send_message(Sender, Addressee, Message),
            Req:respond({200, [{"Content-Type", "text/plain"}], ?OK});
        "/poll" -> 
            Nickname = proplists:get_value("nickname", Params),
            case mucc:poll(Nickname) of 
                {error, Error} -> 
                    Req:respond({500, [{"Content-Type", "text/plain"}], subst("Error: ~s~n", [Error])});
                Messages ->
                    case length(Messages) == 0 of 
                        true -> 
                            Req:respond({200, [{"Content-Type", "text/plain"}], <<"none">>});
                        false -> 
                            Template = lists:fold(fun(_, Acc) -> ["~s~n"|Acc] end, [], Messages),
                            Req:respond({200, [{"Content-Type", "text/plain"}], subst(lists:flatten(Template), Messages)})
                    end
            end;
        "/register" -> 
            Nickname = proplists:get_value("nickname", Params),
            case mucc:register_nickname(Nickname) of 
                ok -> 
                    Req:respond({200, [{"Content-Type", "text/plain"}], ?OK});
                Error -> 
                    Req:respond({500, [{"Content-Type", "text/plain"}], subst("Error: ~s~n", [Error])})
            end;
        Unknown -> 
            Req:respond({404, [{"Content-Type", "text/plain"}], subst("Unknown action: ~s~n", [Unknown])})
    end.

subst(Template, Values) when is_list(Values) -> 
    list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) -> 
    case string:str(Path, "?") of 
        0 -> 
            Path;
        N -> 
            string:substr(Path, 1, string:len(Path) - (N +1))
    end.
