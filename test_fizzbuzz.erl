-module(test_fizzbuzz).

-include_lib("eunit/include/eunit.hrl").

-define(SHORT_3_SEQ, [1, 2, fizz]).
-define(SHORT_5_SEQ, [1, 2, fizz, 4, buzz]).
-define(MED_15_SEQ,  [1, 2, fizz, 4, buzz, fizz, 7, 8, fizz, buzz, 11, fizz, 13, 14, fizzbuzz]).

server_test_() -> 
    {setup, 
        fun() -> fizzbuzz:start() end, 
        fun(Pid) -> fizzbuzz:shutdown(Pid) end,
        fun generate_analyze_tests/1}.

generate_sequence_test_() -> 
    {setup,
        fun() -> fizzbuzz:start() end, 
        fun(Pid) -> fizzbuzz:shutdown(Pid) end,
        fun generate_sequence_tests/1}.

generate_analyze_tests(Pid) -> 
    [?_assertEqual(1, fizzbuzz:calc(Pid, 1)),
     ?_assertEqual(fizz, fizzbuzz:calc(Pid, 3)),
     ?_assertEqual(buzz, fizzbuzz:calc(Pid, 5)),
     ?_assertEqual(fizzbuzz, fizzbuzz:calc(Pid, 15))].

generate_sequence_tests(Pid) -> 
    [?_assertMatch(?SHORT_3_SEQ, fizzbuzz:generate_sequence(Pid, 3)),
     ?_assertMatch(?SHORT_5_SEQ, fizzbuzz:generate_sequence(Pid, 5)),
     ?_assertMatch(?MED_15_SEQ, fizzbuzz:generate_sequence(Pid, 15))].
