%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <mtreskin@metachord.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created :  7 Oct 2011 by Maxim Treskin <mtreskin@metachord.com>
%%%-------------------------------------------------------------------
-module(mtriak).

-include_lib("metalkia_core/include/mt_util.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").

-include("mtriak.hrl").

-export([
  get_pid/0,
  inc_counter/1,
  get_obj_value/2,
  get_obj_value/3,
  get_obj_value_to_modify/2,
  get_obj_value_to_modify/3,
  put_obj_value/4,
  put_obj_value/5,
  delete/2,
  delete/3,
  list_buckets/0,
  list_buckets/1,
  list_keys/1,
  list_keys/2,
  delete_keys/2,
  delete_keys/3
]).

-export([
  rl/0
]).

-define(SERVER, mtriak_srv).

-define(CALL_TO, 15000).

get_pid() ->
  pg2:get_closest_pid(?MTRIAK_WORKER_GROUP).

get_obj_value(Bucket, Key) ->
  get_obj_value(get_pid(), Bucket, Key).

get_obj_value(Pid, Bucket, Key) ->
  gen_server:call(Pid, {get_obj_value, Bucket, Key}, ?CALL_TO).

get_obj_value_to_modify(Bucket, Key) ->
  get_obj_value_to_modify(get_pid(), Bucket, Key).

get_obj_value_to_modify(Pid, Bucket, Key) ->
  gen_server:call(Pid, {get_obj_value_to_modify, Bucket, Key}, ?CALL_TO).

put_obj_value(Object, Data, Bucket, Key) ->
  put_obj_value(get_pid(), Object, Data, Bucket, Key).

put_obj_value(Pid, Object, Data, Bucket, Key) ->
  gen_server:call(Pid, {put_obj_value, Object, Data, Bucket, Key}, ?CALL_TO).

delete(Bucket, Key) ->
  delete(get_pid(), Bucket, Key).

delete(Pid, Bucket, Key) ->
  gen_server:call(Pid, {delete, Bucket, Key}, ?CALL_TO).

list_buckets() ->
  list_buckets(get_pid()).

list_buckets(Pid) ->
  gen_server:call(Pid, {list_buckets}, ?CALL_TO).

list_keys(Bucket) ->
  list_keys(get_pid(), Bucket).

list_keys(Pid, Bucket) ->
  gen_server:call(Pid, {list_keys, Bucket}, ?CALL_TO).

delete_keys(Bucket, Keys) ->
  delete_keys(get_pid(), Bucket, Keys).

delete_keys(Pid, Bucket, Keys) ->
  gen_server:call(Pid, {delete_keys, Bucket, Keys}, ?CALL_TO).

inc_counter(Key)
  when is_binary(Key) ->
  iface_call({inc_counter, Key}).

rl() ->
  mtc:rl(?MODULE).

%% Internal
iface_call(Call) ->
  Pid = get_pid(),
  ?DBG("~p", [Pid]),
  gen_server:call(Pid, Call, ?CALL_TO).
