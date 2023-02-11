%% =============================================================================
%%  rosetta_utils.erl -
%%
%%  Copyright (c) 2022-2023 xentelar. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc This module implements common functions
%% @end
%% -----------------------------------------------------------------------------

-module(rosetta_utils).

-export([decode/1]).
-export([replace/3]).

%% =============================================================================
%% API functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% -----------------------------------------------------------------------------
decode(Line) ->
    Tokens = binary:split(Line, [<<",">>], [global]),
    F = fun(Y, List) -> 
            erlang:append_element(List, Y) 
        end,
    lists:foldl(F, {}, Tokens).

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% -----------------------------------------------------------------------------
replace(Data, REList, Replace) ->
    F = fun(RE, Aux) -> 
        re:replace(Aux, RE, Replace, [global, {return, binary}])
    end,
    lists:foldl(F, Data, REList).

