-module(lock_statem).

-behaviour(gen_statem).

-define(NAME, lock_statem).

%% public API
-export([start_link/1]).
-export([button/1, code_length/0]).

%% statem callbacks
-export([init/1, callback_mode/0, terminate/3, handle_common/3]).
-export([locked/3]).

-type button() :: term().
-type code() :: string().
-type state() :: locked | open.
-type common() :: code_length.
-type ret(A) :: gen_statem:event_handler_result(A).

-record(data, {
    code :: code()
}).

%% public API

-spec start_link(code()) -> gen_statem:start_ret().
start_link(Code) ->
    gen_statem:start_link({local, ?NAME}, ?MODULE, Code, []).

-spec button(Button :: button()) -> ok.
button(Button) ->
    gen_statem:cast(?NAME, {button, Button}).

-spec code_length() -> integer().
code_length() ->
    gen_statem:call(?NAME, code_length).

%% statem callbacks

-spec init(code()) -> {ok, state(), #data{}}.
init(Code) ->
    Data = #data{code = Code},
    {ok, locked, Data}.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

-spec handle_common({call, From :: pid()}, common(), Data :: #data{}) -> ret(#data{}).
handle_common({call, From}, code_length, Data = #data{code = Code}) ->
    {keep_state, Data, [{reply, From, length(Code)}]}.

terminate(_Reason, _State, _Data) -> ok.

%% internal statem callbacks

-spec locked(cast, {button, Button :: button()}, Data :: #data{}) -> ret(locked).
locked(cast, {button, _Button}, Data = #data{}) ->
    {next_state, locked, Data}.
