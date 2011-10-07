%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <mtreskin@metachord.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created :  7 Oct 2011 by Maxim Treskin <mtreskin@metachord.com>
%%%-------------------------------------------------------------------
-module(mtriak_srv).

-behaviour(gen_server).

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").

-export([start_link/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-define(SERVER, ?MODULE).

-record(state, {
          client
         }).

%%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
  RiakNode = mtriak_app:get_env(riak_node, node()),
  {ok, Client} = riak:client_connect(RiakNode),
  ?DBG("Riak Client: ~p", [Client]),
  {ok, #state{client = Client}}.

handle_call({add_post, Post}, _From,
  #state{client = Client} = State) ->
  {Data, Object} =
    case Client:get(<<"posts">>, <<"main">>) of
      {ok, Obj} ->
        {riak_object:get_value(Obj), Obj};
      _ ->
        {[], undefined}
    end,

  Key = counter,
  Id = proplists:get_value(Key, Data, 1),
  NewId = Id+1,
  NewData = lists:keystore(Key, 1, Data, {Key, NewId}),

  NewObj =
    if Object =:= undefined ->
        riak_object:new(<<"posts">>, <<"main">>, NewData);
       true ->
        riak_object:update_value(Object, NewData)
    end,

  Client:put(NewObj, 3),

  PostObj = riak_object:new(<<"posts">>, iolist_to_binary(integer_to_list(NewId)), Post),
  Client:put(PostObj, 3),

  {reply, Id, State};

handle_call({add_comment, PostId, ParentId, #mt_comment{parents = Parents} = Comment}, _From,
  #state{client = Client} = State) ->
  ?DBG("PostId: ~p~nParentId: ~p~nComment: ~p", [PostId, ParentId, Comment]),
  {ok, PostObj} = Client:get(<<"posts">>, iolist_to_binary(PostId)),
  #mt_post{
    comments_cnt = CommCnt,
    comments = Comments
  } = Post = riak_object:get_value(PostObj),

  NewId = CommCnt+1,
  NewPost = Post#mt_post{
    comments_cnt = NewId,
    comments = [Comment#mt_comment{parents = Parents ++ [NewId]} | Comments]
  },

  NewObj = riak_object:update_value(PostObj, NewPost),
  Client:put(NewObj, 3),

  {reply, NewId, State};


handle_call({get_post, PostId}, _From,
  #state{client = Client} = State) ->
  ?DBG("PostId: ~p", [PostId]),
  Result =
  case Client:get(<<"posts">>, iolist_to_binary(PostId)) of
    {ok, PostObj} ->
      riak_object:get_value(PostObj);
    Error ->
      Error
  end,
  {reply, Result, State};

handle_call({get_posts, PostId}, _From,
  #state{client = Client} = State) ->
  ?DBG("PostId: ~p", [PostId]),
  Result =
  case Client:get(<<"posts">>, iolist_to_binary(PostId)) of
    {ok, PostObj} ->
      riak_object:get_value(PostObj);
    Error ->
      Error
  end,
  {reply, Result, State};

handle_call(Request, _From, State) ->
  Error = {unknown_call, Request},
  {stop, Error, {error, Error}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions
