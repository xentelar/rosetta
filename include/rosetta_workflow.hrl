%% =============================================================================
%%  rosetta_workflow.hrl -
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
%% @doc workflow model
%% @end
%% -----------------------------------------------------------------------------
-record(workflow, {
    process_name        :: binary(),
    template            :: binary(),
    module              :: binary(),
    space               :: binary(),
    events              :: map(),
    states              :: map()}
).

%% -----------------------------------------------------------------------------
%% @doc state model
%% @end
%% -----------------------------------------------------------------------------
-record(state, {
    state               :: binary(),
    action              :: tuple(),
    transitions         :: list()}
).

%% -----------------------------------------------------------------------------
%% @doc action model
%% @end
%% -----------------------------------------------------------------------------
-record(action, {
    module              :: binary() | undefined,
    space          :: binary() | undefined}
).

%% -----------------------------------------------------------------------------
%% @doc transition model
%% @end
%% -----------------------------------------------------------------------------
-record(transition, {
    state               :: binary(),
    next_state          :: binary(),
    event               :: binary()}
).