%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


-module(mtriak_worker).

-behaviour(gen_server).

-include_lib("metalkia_core/include/mt_log.hrl").

-include("mtriak.hrl").

-export([start_link/1]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-define(SERVER, ?MODULE).

-define(PG2_JOIN, (ok = pg2:join(?MTRIAK_WORKER_GROUP, self()))).
-define(PG2_LEAVE, (ok = pg2:leave(?MTRIAK_WORKER_GROUP, self()))).
-define(PG2_REJOIN_TO, 10000).

-record(state, {
          node     :: node(),
          client   :: pid(),
          joined   :: boolean(),
          replicas :: [{binary(), integer()}]
         }).

%%% API
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%%% gen_server callbacks

init(Args) ->
  Node = proplists:get_value(node, Args, node()),
  ?DBG("Worker for ~p", [Node]),
  {ok, Client} = riak:client_connect(Node),
  ?DBG("Riak Client: ~p", [Client]),
  Replicas = mtriak_app:get_env(bucket_replicas, []),
  ?PG2_JOIN,
  {ok, #state{
     node = Node,
     client = Client,
     joined = true,
     replicas = Replicas
    }}.


get_obj_value(Client, Bucket, Key) ->
  case Client:get(Bucket, Key) of
    {ok, Obj} ->
      {riak_object:get_value(Obj), Obj};
    _ ->
      {undefined, undefined}
  end.


handle_call({get_obj_value, Bucket, Key}, _From,
            #state{client = Client} = State) ->
  {Data, _Object} = get_obj_value(Client, Bucket, Key),
  {reply, Data, State};

handle_call({get_obj_value_to_modify, Bucket, Key}, _From,
            #state{client = Client} = State) ->
  ?PG2_LEAVE,
  {Data, Object} = get_obj_value(Client, Bucket, Key),
  {reply, {Data, Object}, State#state{joined = false}, ?PG2_REJOIN_TO};

handle_call({put_obj_value, Object, Data, Bucket, Key}, _From,
            #state{client = Client} = State) ->
  NewObj =
    if Object =:= undefined ->
        riak_object:new(Bucket, Key, Data);
       true ->
        riak_object:update_value(Object, Data)
    end,
  Client:put(NewObj, get_replicas(Bucket, State)),
  NewState =
    if State#state.joined ->
        State;
       true ->
        ?PG2_JOIN,
        State#state{joined = true}
    end,
  {reply, ok, NewState};

handle_call({delete, Bucket, Key}, _From,
            #state{client = Client} = State) ->
  Client:delete(Bucket, Key, get_replicas(Bucket, State)),
  {reply, ok, State};

handle_call({inc_counter, Key}, _From,
            #state{client = Client} = State) ->
  Bucket = <<"counters">>,
  {{counter, Id} = _Data, Object} =
    case Client:get(Bucket, Key) of
      {ok, Obj} ->
        {riak_object:get_value(Obj), Obj};
      _ ->
        {{counter, 0}, undefined}
    end,
  NewData = {counter, Id+1},
  NewObj =
    if Object =:= undefined ->
        riak_object:new(Bucket, Key, NewData);
       true ->
        riak_object:update_value(Object, NewData)
    end,
  Client:put(NewObj, get_replicas(Bucket, State)),
  {reply, Id, State};

handle_call({list_buckets}, _From,
            #state{client = Client} = State) ->
  Keys = Client:list_buckets(),
  {reply, Keys, State};

handle_call({list_keys, Bucket}, _From,
            #state{client = Client} = State) ->
  Keys = Client:list_keys(Bucket),
  {reply, Keys, State};

handle_call({delete_keys, Bucket, Keys}, _From,
            #state{client = Client} = State) ->
  [Client:delete(Bucket, Key) || Key <- Keys],
  {reply, ok, State};

handle_call(Request, _From, State) ->
  Error = {unknown_call, Request},
  {stop, Error, {error, Error}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  ?ERR("PG2 rejoin timeout fired", []),
  ?PG2_JOIN,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions
get_replicas(Bucket, State) ->
  proplists:get_value(Bucket, State#state.replicas, 3).
