%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%%%-------------------------------------------------------------------

-module(rosetta_app).

-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ?LOG_INFO("\nStart Rosetta", []),
    rosetta_processor:process(),
    rosetta_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
