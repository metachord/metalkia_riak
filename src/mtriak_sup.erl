%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <mtreskin@metachord.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created :  7 Oct 2011 by Maxim Treskin <mtreskin@metachord.com>
%%%-------------------------------------------------------------------
-module(mtriak_sup).

-include_lib("metalkia_core/include/mt_log.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  ?DBG("Start Metalk riak sup", []),
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {mtriak_srv, {mtriak_srv, start_link, []},
            Restart, Shutdown, Type, [mtriak_srv]},

  {ok, {SupFlags, [AChild]}}.

%%% Internal functions
