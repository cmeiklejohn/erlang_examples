-module(message_store).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0, save_message/2, find_messages/1, shutdown/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-record(chat_message, {addressee, body, created_on}). 

%%%===================================================================
%%% API
%%%===================================================================

save_message(Addressee, Body) -> 
    gen_server:call(?SERVER, {save_message, Addressee, Body}).

find_messages(Addressee) ->
    case gen_server:call(?SERVER, {find_messages, Addressee}) of
        {ok, Messages} -> 
            Messages
    end.

shutdown() -> 
    gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    init_store(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({save_message, Addressee, Body}, _From, State) -> 
    store_message(Addressee, Body),
    {reply, ok, State};

handle_call({find_messages, Addressee}, _From, State) -> 
    Messages = get_messages(Addressee),
    {reply, {ok, Messages}, State};

handle_call(stop, _From, State) ->
    mnesia:stop(), 
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    Reply = ignored,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

store_message(Addressee, Body) -> 
    F = fun() -> 
            {_, CreatedOn, _} = erlang:now(),
            mnesia:write(#chat_message{addressee=Addressee, body=Body, created_on=CreatedOn}) end,
    mnesia:transaction(F).

get_messages(Addressee) -> 
    F = fun() -> 
            Query = qlc:q([M#chat_message.body || M <- mnesia:table(chat_message), M#chat_message.addressee =:= Addressee]),
            Results = qlc:e(Query),
            delete_messages(Results),
            Results end,
    {atomic, Messages} = mnesia:transaction(F), 
    Messages.

delete_messages(Messages) -> 
    F = fun() -> 
            lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages) end,
    mnesia:transaction(F).

init_store() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    try
        mnesia:table_info(chat_message, type)
    catch
        exit: _ -> 
            mnesia:create_table(chat_message, [{attributes, record_info(fields, chat_message)},
                                               {type, bag},
                                               {disc_copies, [node()]}])
    end.
