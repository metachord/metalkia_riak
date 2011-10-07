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
-include_lib("metalkia_core/include/mt_log.hrl").

-export([
  add_post/1,
  add_comment/3,
  get_post/1
]).

-define(SERVER, mtriak_srv).

add_post(Post) when is_record(Post, mt_post) ->
  gen_server:call(?SERVER, {add_post, Post}).

get_post(PostId) ->
  gen_server:call(?SERVER, {get_post, PostId}).

add_comment(PostId, ParentId, Comment) when is_record(Comment, mt_comment) ->
  gen_server:call(?SERVER, {add_comment, PostId, ParentId, Comment}).
