%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


-module(mtriak_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([get_env/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mtriak_sup:start_link().

stop(_State) ->
    ok.

get_env(Key, Default) ->
  gproc:get_env(l, metalkia_riak, Key, [app_env, {default, Default}]).
