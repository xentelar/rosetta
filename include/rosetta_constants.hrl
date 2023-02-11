%% =============================================================================
%%  rosetta_constants.hrl -
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
%% @doc regular expressions
%% @end
%% -----------------------------------------------------------------------------

-define(RE_WHITE_SPACE, [<<"\\s+">>, <<"\\\\n">>] ).

-define(RE_STATE_REMOVE, [<<"state\"">>, <<">>">>] ).

-define(RE_NOTE_REMOVE, [<<"note\"">>, <<"fsmname:">>] ).

-define(RE_STATE_REPLACE, [<<"\"as">>, <<"<<package:">>, <<"<<class:">>] ).

-define(RE_NODE_REPLACE, [<<"<<([^<]*)>">>, <<"-([^-]*)->">>, <<":">>] ).

-define(RE_NOTE_REPLACE, [<<"\"as">>, <<"fsmtemplate:">>, <<"fsmpackage:">>, <<"fsmclass:">>] ).

%% -----------------------------------------------------------------------------
%% @doc constants
%% @end
%% -----------------------------------------------------------------------------

-define(NOTHING, <<"">> ).

-define(COMMA, <<",">> ).