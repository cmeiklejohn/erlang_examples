-module(misc_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, add/2, subtract/2, add_and_subtract/2, stop/0]).

%% Client functions

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(First, Second) -> 
    gen_server:call(?MODULE, {add, First, Second}).

subtract(First, Second) -> 
    gen_server:call(?MODULE, {subtract, First, Second}).

add_and_subtract(First, Second) -> 
    [misc_server:add(First, Second), misc_server:subtract(First, Second)].

stop() -> 
    gen_server:cast(?MODULE, stop).

%% gen_server behaviour

init([]) -> 
    {ok, []}.

handle_call({add, First, Second}, _From, State) -> 
    {reply, {ok, First + Second}, State};

handle_call({subtract, First, Second}, _From, State) -> 
    {reply, {ok, First - Second}, State};

handle_call(_Request, _From, State) -> 
    {reply, ignored, State}.

handle_cast(stop, State) -> 
    {stop, normal, State};

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    io:format("Info message received: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) -> 
    io:format("Server stopped.~n"),
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.
