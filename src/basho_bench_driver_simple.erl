-module(basho_bench_driver_simple).
-author("Gonçalo Tomás <goncalo@goncalotomas.com>").

-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-define (MAX, 10000000).
-define (NUM_BYTES, 64).

%% ====================================================================
%% API
%% ====================================================================


new(_Id) ->
    {ok, []}.

run(add_two_numbers, _KeyGen, _ValueGen, _State) ->
    _ = random_number()+random_number(),
    {ok, []};
run(subtract_two_numbers, _KeyGen, _ValueGen, _State) ->
    _ = random_number()-random_number(),
    {ok, []};
run(get_random_string, _KeyGen, _ValueGen, _State) ->
    _ = base64:encode(crypto:strong_rand_bytes(?NUM_BYTES)),
    {ok, []}.

random_number() ->
    rand:uniform(?MAX).
