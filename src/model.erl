-module(model).
-export([start/3]).
-export([join/0]).
-export([join/1]).
-export([sub/1]).
-export([new/1]).
-export([new/2]).
-export([append/2]).
-export([prepend/2]).
-export([clear/1]).
-export([set/2]).
-export([get/1]).

-include("recs.hrl").

start(AppName, Mod, Port) ->
  application:start(mnesia),
  mnesia:create_table(model, [{attributes, record_info(fields, model)}]),
  mnesia:create_table(reg, [{attributes, record_info(fields, reg)}]),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, AppName, "templates/main.html"}},
      {"/assets/[...]", cowboy_static, {priv_dir, AppName, "assets"}},
      {"/ws", ui_client, Mod}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
    [{port, Port}],
    #{env => #{dispatch => Dispatch}}
  ).

join() ->
  join(self()).

join(Delegate) ->
  mnesia:dirty_write(#reg{id = self(), delegate = Delegate}).

new(Id) ->
  new(Id, []).

new(Id, Props) ->
  case mnesia:dirty_read(reg, self()) of
    [#reg{delegate = Delegate}] ->
      Init = case proplists:get_value(scalar, Props, false) of
        true -> undefined;
        false -> queue:new()
      end,
      Model = #model{id = Id, data = Init, subs = [Delegate]},
      mnesia:dirty_write(Model),
      {ok, new};
    [] -> {failed, not_joined}
  end.

sub(Id) ->
  case mnesia:dirty_read(reg, self()) of
    [#reg{delegate = Delegate}] ->
      F = fun() ->
        case mnesia:read(model, Id) of
          [#model{data = Data, subs = Subs} = Model] ->
            mnesia:write(Model#model{subs = [Delegate | Subs]}),
            notify({set, Id, Data}, Subs),
            {ok, sub};
          [] ->
            {failed, nomodel}
        end
      end,
      case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {failed, Reason}
      end;
    [] -> {failed, not_joined}
  end.

append(Item, Id) ->
  F = fun() ->
    case mnesia:read(model, Id) of
      [#model{data = Data, subs = Subs} = Model] ->
        Data2 = queue:in(Item, Data),
        mnesia:write(Model#model{data = Data2}),
        notify({append, Id, Item}, Subs),
        {ok, append};
      [] ->
        {failed, nomodel}
    end
  end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> {failed, Reason}
  end.

prepend(Item, Id) ->
  F = fun() ->
    case mnesia:read(model, Id) of
      [#model{data = Data, subs = Subs} = Model] ->
        Data2 = queue:in_r(Item, Data),
        mnesia:write(Model#model{data = Data2}),
        notify({prepend, Id, Item}, Subs),
        {ok, append};
      [] ->
        {failed, nomodel}
    end
  end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> {failed, Reason}
  end.

clear(Id) ->
  set(queue:new(), Id).

set(Value, Id) when is_list(Value) ->
  set(queue:from_list(Value), Id);

set(Value, Id) ->
  F = fun() ->
    case mnesia:read(model, Id) of
      [#model{subs = Subs} = Model] ->
        mnesia:write(Model#model{data = Value}),
        notify({set, Id, Value}, Subs),
        {ok, append};
      [] ->
        {failed, nomodel}
    end
  end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> {failed, Reason}
  end.

get(Id) ->
  F = fun() ->
    case mnesia:read(model, Id) of
      [#model{data = Data}] ->
        case queue:is_queue(Data) of
          true -> {ok, queue:to_list(Data)};
          false ->
            {ok, Data}
        end;
      [] ->
        {failed, nomodel}
    end
  end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> {failed, Reason}
  end.

notify(Message, Subs) ->
  [Sub ! {model, Message} || Sub <- Subs].
