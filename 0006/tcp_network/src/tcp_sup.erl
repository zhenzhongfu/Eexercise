%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%

-module(tcp_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, start_child/2, start_child/3,
         start_supervisor_child/1, start_supervisor_child/2,
         start_supervisor_child/3,
         start_restartable_child/1, start_restartable_child/2,
         start_delayed_restartable_child/1, start_delayed_restartable_child/2,
         stop_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(MAX_WAIT, 16#ffffffff). 

%%----------------------------------------------------------------------------

start_link() -> 
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Mod) -> start_child(Mod, []).

start_child(Mod, Args) -> start_child(Mod, Mod, Args).

start_child(ChildId, Mod, Args) ->
    child_reply(supervisor:start_child(
                  ?SERVER,
                  {ChildId, {Mod, start_link, Args},
                   transient, ?MAX_WAIT, worker, [Mod]})).

start_supervisor_child(Mod) -> start_supervisor_child(Mod, []).

start_supervisor_child(Mod, Args) -> start_supervisor_child(Mod, Mod, Args).

start_supervisor_child(ChildId, Mod, Args) ->
    child_reply(supervisor:start_child(
                  ?SERVER,
                  {ChildId, {Mod, start_link, Args},
                   transient, infinity, supervisor, [Mod]})).

start_restartable_child(M)            -> start_restartable_child(M, [], false).
start_restartable_child(M, A)         -> start_restartable_child(M, A,  false).
start_delayed_restartable_child(M)    -> start_restartable_child(M, [], true).
start_delayed_restartable_child(M, A) -> start_restartable_child(M, A,  true).

start_restartable_child(Mod, Args, Delay) ->
    Name = list_to_atom(atom_to_list(Mod) ++ "_sup"),
    child_reply(supervisor:start_child(
                  ?SERVER,
                  {Name, {tcp_sup, start_link,
                          [Name, {Mod, start_link, Args}, Delay]},
                   transient, infinity, supervisor, [tcp_sup]})).

stop_child(ChildId) ->
    case supervisor:terminate_child(?SERVER, ChildId) of
        ok -> supervisor:delete_child(?SERVER, ChildId);
        E  -> E
    end.

init([]) -> {ok, {{one_for_all, 0, 1}, []}}.


%%----------------------------------------------------------------------------

child_reply({ok, _}) -> ok;
child_reply(X)       -> X.
