%% =============================================================================
%%  rosetta_processor.erl -
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
%% @doc 
%% @end
%% -----------------------------------------------------------------------------

-module(rosetta_processor).

-include_lib("kernel/include/logger.hrl").

-include("rosetta_workflow.hrl").
-include("rosetta_constants.hrl").

-export([process/1]).

%% =============================================================================
%% API functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc read and process the puml file and get sate machime model
%% @end
%% -----------------------------------------------------------------------------
process(FileName) ->

   Start = #state{state = <<"start">>, 
                  action = #action{}, 
                  transitions = []},

   End = #state{state = <<"end">>, 
               action = #action{}, 
               transitions = []},

   InitialWorkflow = #workflow{events = #{}, 
                              states = #{<<"start">> => Start, <<"end">> => End}},

   FileContent = read_lines(FileName),
   ?LOG_INFO("File Name --> ~p", [FileName]),
   F = fun(Line0, Workflow) ->
         Line = string:trim(Line0, leading),
         Workflow0 = process_state(Line, state(Line), Workflow),
         Workflow1 = process_transition(Line, transition(Line), Workflow0),
         process_note(Line, note(Line), Workflow1)
      end,
   lists:foldl(F, InitialWorkflow, FileContent).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc load all file content
%% @end
%% @private
%% -----------------------------------------------------------------------------
read_lines(FileName) ->
   {ok, Data} = file:read_file(FileName),
   Data0 = rosetta_utils:replace(Data, [<<" \\\\\\n">>], ?NOTHING),
   binary:split(Data0, [<<"\n">>], [global]).

%% -----------------------------------------------------------------------------
%% @doc looking for state keyword in fsm model
%% @end
%% @private
%% -----------------------------------------------------------------------------
state(Line) -> string:find(Line, <<"state \"">>, leading).

%% -----------------------------------------------------------------------------
%% @doc looking for transitions states in fsm model
%% @end
%% @private
%% -----------------------------------------------------------------------------
transition(Line) -> re:run(Line, <<"-([^-]*)->">>).

%% -----------------------------------------------------------------------------
%% @doc looking for note keyword in fsm model
%% @end
%% @private
%% -----------------------------------------------------------------------------
note(Line) -> string:find(Line, <<"note \"">>, leading).

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% @private
%% -----------------------------------------------------------------------------
process_state(_, nomatch, Workflow) ->
   Workflow;
process_state(Line, _R, Workflow) ->
   #workflow{states = States} = Workflow,
   %?LOG_INFO("LINE IN --> ~p", [Line]),
   Line0 =
      rosetta_utils:replace(Line, ?RE_WHITE_SPACE ++ ?RE_STATE_REMOVE, ?NOTHING),
   Line1 =
      rosetta_utils:replace(Line0, ?RE_STATE_REPLACE, ?COMMA),
   %?LOG_INFO("LINE OUT --> ~p", [Line1]),
   {State0, VertPack, VertClass, _State1} = rosetta_utils:decode(Line1),

   NewState = case maps:find(State0, States) of
               error -> 
                  Action = #action{space = VertPack, module = VertClass},
                  #state{state = State0, action = Action, transitions = []};
               _ ->
                  ?LOG_ERROR("ERROR state is duplicate: --> ~p", [State0]),
                  throw(<<"ERROR state is duplicate">>)
            end,

   States0 = maps:put(State0, NewState, States),
   Workflow#workflow{states = States0}.

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% @private
%% -----------------------------------------------------------------------------
process_transition(_, nomatch, Workflow) ->
   Workflow;
process_transition(Line, _R, Workflow) ->
   #workflow{states = States, 
            events = Events} = Workflow,

   %?LOG_INFO("LINE IN --> ~p", [Line]),
   Line0 = 
      rosetta_utils:replace(Line, ?RE_WHITE_SPACE, ?NOTHING),
   Line1 = 
      rosetta_utils:replace(Line0, ?RE_NODE_REPLACE, ?COMMA),
   %?LOG_INFO("LINE OUT --> ~p", [Line1]),
   {S, NextStateId, Event} = rosetta_utils:decode(Line1),
   StateId = n_start(S),

   State = maps:find(StateId, States),

   NewState =
      create_transitions(StateId, n_end(NextStateId), Event, State),

   Events0 = maps:put(Event, Event, Events),
   States0 = maps:put(StateId, NewState, States),

   Workflow#workflow{states = States0, 
                     events = Events0}.

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% @private
%% -----------------------------------------------------------------------------
process_note(_, nomatch, Workflow) ->
   Workflow;
process_note(Line, _R, Workflow) ->
   Line1 =
      rosetta_utils:replace(Line, ?RE_WHITE_SPACE ++ ?RE_NOTE_REMOVE, ?NOTHING),
   Line2 =
      rosetta_utils:replace(Line1, ?RE_NOTE_REPLACE, ?COMMA),
   {FsmName, FsmTemplate, FsmSpace, FsmModule, _} = rosetta_utils:decode(Line2),
   Workflow#workflow{process_name = FsmName,
                     template = FsmTemplate,
                     space = FsmSpace,
                     module = FsmModule}.

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% @private
%% -----------------------------------------------------------------------------
n_start(<<"[*]">>) -> <<"start">>;
n_start(V) -> V.

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% @private
%% -----------------------------------------------------------------------------
n_end(<<"[*]">>) -> <<"end">>;
n_end(V) -> V.

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% @private
%% -----------------------------------------------------------------------------
create_transitions(StateId, _NextStateId, _Event, error) ->
   ?LOG_ERROR("ERROR node does not contain state:  --> ~p", [StateId]),
         throw(<<"ERROR node does not contail state">>);

create_transitions(StateId, NextStateId, Event, {ok, State}) ->
   #state{transitions = Transitions} = State,
   T = Transitions
      ++ [#transition{state = StateId,
                     event = Event,
                     next_state = NextStateId}],
   State#state{transitions = T}.