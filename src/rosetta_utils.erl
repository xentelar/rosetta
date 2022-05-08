%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%%%-------------------------------------------------------------------

-module(rosetta_utils).

-export([decode/1]).
-export([replace/3]).

decode(Line) ->
    Tokens = binary:split(Line, [<<",">>], [global]),
    F = fun(Y, List) -> 
            erlang:append_element(List, Y) 
        end,
    lists:foldl(F, {}, Tokens).

replace(Data, REList, Replace) ->
    F = fun(RE, Aux) -> 
        re:replace(Aux, RE, Replace, [global, {return, binary}])
    end,
    lists:foldl(F, Data, REList).

