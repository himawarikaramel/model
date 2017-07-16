-module(ui_client).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
	{cowboy_websocket, Req, State}.

websocket_init(Mod) ->
  {ok, Pid} = ui:start_link(Mod, self()),
	{ok, #{ui => Pid}}.

websocket_handle({text, Data}, State) ->
	{reply, {text, Data}, State};
websocket_handle({binary, <<131, _/binary>> = Data}, State) ->
  handle(erlang:binary_to_term(Data, [safe]), State),
	{ok, State};
websocket_handle({binary, Data}, State) ->
	{reply, {binary, Data}, State};
websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info({ui, _} = Reply, State) ->
	{reply, {binary, erlang:term_to_binary(Reply)}, State};

websocket_info({model, _} = Reply, State) ->
	{reply, {binary, erlang:term_to_binary(Reply)}, State};

websocket_info({reply, Term}, State) ->
	{reply, {binary, erlang:term_to_binary(Term)}, State};

websocket_info(_Info, State) ->
	{ok, State}.

handle({ui, _Event} = Message, #{ui := UI}) ->
  gen_server:cast(UI, Message);

handle(_Message, _State) -> ok.
