%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <mtreskin@metachord.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created :  7 Oct 2011 by Maxim Treskin <mtreskin@metachord.com>
%%%-------------------------------------------------------------------
-module(mtriak).

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").

-include("mtriak.hrl").

-export([
  get_pid/0,
  add_post/1,
  add_comment/2,
  get_post/1,
  inc_counter/1,
  get_obj_value/2,
  get_obj_value/3,
  get_obj_value_to_modify/2,
  get_obj_value_to_modify/3,
  put_obj_value/4,
  put_obj_value/5
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

add_post(Post) when is_record(Post, mt_post) ->
  iface_call({add_post, Post}).

get_post(PostId) ->
  iface_call({get_post, ?a2b(PostId)}).

add_comment(PostId, Comment) when is_record(Comment, mt_comment) ->
  iface_call({add_comment, ?a2b(PostId), Comment}).

inc_counter(Key)
  when is_binary(Key) ->
  iface_call({inc_counter, Key}).

%% Internal
iface_call(Call) ->
  Pid = get_pid(),
  ?DBG("~p", [Pid]),
  gen_server:call(Pid, Call, ?CALL_TO).
