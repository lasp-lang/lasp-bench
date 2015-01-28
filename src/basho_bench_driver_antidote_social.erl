%% -------------------------------------------------------------------
%%
%% basho_bench: Benchmarking Suite
%%
%% Copyright (c) 2009-2010 Basho Techonologies
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_bench_driver_antidote_social).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-define(TIMEOUT, 20000).
-record(state, {node,
                worker_id,
                time,
                type_dict,
                user_id,
                num_users,
                friends}).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    %% Make sure bitcask is available
    case code:which(antidote) of
        non_existing ->
            ?FAIL_MSG("~s requires antidote to be available on code path.\n",
                      [?MODULE]);
        _ ->
            ok
    end,

    Nodes   = basho_bench_config:get(antidote_nodes),
    Cookie  = basho_bench_config:get(antidote_cookie),
    MyNode  = basho_bench_config:get(antidote_mynode, [basho_bench, longnames]),
    NumUsers  = basho_bench_config:get(antidote_numclients),

    %% Try to spin up net_kernel
    case net_kernel:start(MyNode) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
    end,

    %% Initialize cookie for each of the nodes
    true = erlang:set_cookie(node(), Cookie),
    [true = erlang:set_cookie(N, Cookie) || N <- Nodes],

    %% Try to ping each of the nodes
    ping_each(Nodes),

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(Nodes)+1), Nodes),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),
    
    {ok, UserId} = social:create_user(string:concat("User", integer_to_list(Id)), TargetNode),
    {ok, #state{node=TargetNode, time={1,1,1}, worker_id=Id,  user_id = UserId, num_users = NumUsers, friends = []}}.

%% @doc Read a key
run(read_msg, _KeyGen, _ValueGen, State=#state{node=Node, user_id = UserId}) ->
    Response = social:read_message(UserId, Node),
    case Response of
        {ok, _Value} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(update_status, _KeyGen, _ValueGen, State=#state{node=Node, user_id = UserId}) ->
    Response = social:update_status(UserId, "Hello world", Node),
    case Response of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(send_friend_request, _KeyGen, _ValueGen, State=#state{node=Node, user_id = UserId, num_users=NumUsers, friends = Friends}) ->
    random:seed(now()),
    NumFrnd = random:uniform(NumUsers),
    case NumFrnd =:= UserId of 
        true -> {ok, State};
        false ->
            Response = 
                case lists:member(NumFrnd, Friends) of 
                    false -> social:send_friend_request(UserId, NumFrnd, Node);
                    true -> {ok, nothing_to_do}
                end,
            case Response of
                ok ->    
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State};
                {badrpc, Reason} ->
                    {error, Reason, State}
            end            
    end;        

run(answer_friend_request,  _KeyGen, _ValueGen, State=#state{node=Node, user_id = UserId, friends = Friends}) ->
    case social:read_friend_requests_in(UserId, Node) of
        {ok, Ids} -> 
            case length(Ids) > 0 of
                true -> 
                    First = hd(Ids),
                    Response = social:answer_friend_request(UserId, First, true, Node),
                    case Response of
                        ok ->                     
                            {ok, State#state{friends = Friends ++ [First]}};
                        {error, Reason} ->
                            {error, Reason, State};
                        {badrpc, Reason} ->
                            {error, Reason, State}
                    end;
                false -> 
                    {ok, State}
            end;
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;        

run(read_friend_list, _KeyGen, _ValueGen, State=#state{node=Node, user_id = UserId}) ->
    case social:read_friend_list(UserId, Node) of
        {ok, _Friends} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(post_message,  _KeyGen, _ValueGen, State=#state{node=Node, user_id = UserId, friends = Friends}) ->
    %% get random friend from friends
    %% post message
    random:seed(now()),
    case length(Friends) > 0 of
        true ->
            Num = random:uniform(length(Friends)),
            Friend = lists:nth(Num, Friends),
            Response = social:post_message(UserId, "Hi", Friend, Node),
            case Response of
                ok ->                     
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State};  
                {badrpc, Reason} ->
                    {error, Reason, State}
            end;
        false ->
            {ok, State}
    end.

%% Private
ping_each([]) ->
    ok;
ping_each([Node | Rest]) ->
    case net_adm:ping(Node) of
        pong ->
            ?INFO("Finished pinging ~p", [Node]),
            ping_each(Rest);
        pang ->
            ?FAIL_MSG("Failed to ping node ~p\n", [Node])
    end.
