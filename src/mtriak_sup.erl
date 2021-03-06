%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


-module(mtriak_sup).

-include_lib("metalkia_core/include/mt_log.hrl").

-behaviour(supervisor).

-export([
  start_link/0,
  start_worker/1
]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker(Args) ->
  supervisor:start_child(mtriak_worker_sup, [Args]).

init([mtriak_worker]) ->
  ?DBG("DB Worker supervisor start", []),
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 100,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Children =
    [
     {mtriak_worker, {mtriak_worker, start_link, []},
      transient, 10000, worker, [mtriak_worker]}
    ],
  {ok, {SupFlags, Children}};

init([]) ->
  ?DBG("Start Metalk riak sup", []),
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Children =
    [
     {mtriak_worker_sup,
      {supervisor, start_link,
       [{local, mtriak_worker_sup}, ?MODULE, [mtriak_worker]]},
      permanent, 20000, supervisor, [?MODULE]},
     {mtriak_srv, {mtriak_srv, start_link, []},
      permanent, 2000, worker, [mtriak_srv]}
    ],

  {ok, {SupFlags, Children}}.

%%% Internal functions
