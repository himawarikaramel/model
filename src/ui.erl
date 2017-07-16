-module(ui).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([start_link/2]).
-export([emit/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-callback ui_init() -> {ok, term()} | {failed, term()}.
-callback ui_event(Event :: term(), State :: term()) -> {ok, term()} | {stop, term(), term()} | {failed, term()}.

-include("recs.hrl").

%% API.

-spec start_link(Config :: term()) -> {ok, pid()}.
start_link(#{} = Conf) ->
	gen_server:start_link(?MODULE, Conf, []);
start_link(Mod) ->
	gen_server:start_link(?MODULE, Mod, []).
-spec start_link(Mod :: atom(), Delegate :: pid()) -> {ok, pid()}.
start_link(Mod, Delegate) ->
	gen_server:start_link(?MODULE, #{mod => Mod, delegate => Delegate}, []).

emit(Event) ->
	case mnesia:dirty_read(reg, self()) of
		[#reg{delegate = Delegate}] -> Delegate ! {ui, Event};
		[] -> {failed, not_joined}
	end.

%% gen_server.

init(#{mod := Mod, delegate := Delegate}) ->
	model:join(Delegate),
	{ok, UiState} = Mod:ui_init(),
	{ok, #{mod => Mod, ui_state => UiState}};

init(Mod) ->
	init(#{mod => Mod, delegate => self()}).

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({ui, Event}, #{mod := Mod, ui_state := UiState} = State) ->
	case Mod:ui_event(Event, UiState) of
		{ok, UiState2} -> {noreply, State#{ui_state := UiState2}};
		{stop, Reason, UiState2} -> {stop, Reason, State#{ui_state := UiState2}};
		{failed, Reason} -> {stop, Reason, State}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
