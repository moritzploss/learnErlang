-module(bike_gen_server).

-behaviour(gen_server).

-export([init/1, start_link/0]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).
-export([order_bike/3, return_bike/2, get_bikes/1, close_shop/1]).

-record(bike, {
    model :: string(),
    manufacturer :: string()
}).

%% external API

init([]) ->
    {ok, []}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

order_bike(Pid, Model, Manufacturer) ->
    gen_server:call(Pid, {order, Model, Manufacturer}).

return_bike(Pid, Bike = #bike{}) ->
    gen_server:cast(Pid, {return, Bike}).

get_bikes(Pid) ->
    gen_server:call(Pid, get_all).

close_shop(Pid) ->
    gen_server:call(Pid, terminate).

%% internal callbacks

handle_call({order, Model, Manufacturer}, _From, Bikes) ->
    Bike = #bike{model = Model, manufacturer = Manufacturer},
    {reply, {ok, Bike}, Bikes};
handle_call(get_all, _From, Bikes) ->
    {reply, Bikes, Bikes};
handle_call(terminate, _From, Bikes) ->
    {stop, normal, ok, Bikes}.

handle_cast({return, Bike = #bike{}}, Bikes) ->
    {noreply, [Bike | Bikes]}.

handle_info(Msg, Bikes) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, Bikes}.

terminate(normal, Bikes) ->
    [io:format("~p bike is still for sale.~n", [B#bike.model]) || B <- Bikes],
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
