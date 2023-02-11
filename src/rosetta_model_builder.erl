%% =============================================================================
%%  rosetta_model_builder.erl -
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

-module(rosetta_model_builder).

-include_lib("kernel/include/logger.hrl").

-include("rosetta_workflow.hrl").

-export([build/3]).

%% =============================================================================
%% API functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc build the model
%% @end
%% -----------------------------------------------------------------------------
build(SourcePath, OutPath, Workflow) ->
   TemplateName = Workflow#workflow.template,

   FSMTemplateFile = list_to_binary([SourcePath, TemplateName]),
   VCTemplateFile = list_to_binary([SourcePath, <<"verticle.mustache">>]),
   VBCTemplateFile = list_to_binary([SourcePath, <<"base-verticle.mustache">>]),

   ?LOG_INFO("Template Name --> ~p", [FSMTemplateFile]),
   FSMTemplate = get_template(FSMTemplateFile),
   VCTemplate = get_template(VCTemplateFile),
   VBCTemplate = get_template(VBCTemplateFile),

   #workflow{states = States} = Workflow,

   S = fun(_Key, State, Aux) -> 
         #state{transitions = Transitions} = State,
         FL = fun(T, Accu) ->
                  Accu ++ [#{"next_state" => T#transition.next_state,
                              "event" => T#transition.event,
                              "is_next_end" => is_end(T#transition.next_state)}]
               end,
         Transitions0 = lists:foldl(FL, [], Transitions),
         Aux ++ [#{"state" => State#state.state,
                  "is_end" => is_end(State#state.state),
                  "is_start" => is_start(State#state.state),
                  "class" => State#state.action#action.module,
                  "package" => State#state.action#action.space,
                  "transitions" => Transitions0}]
      end,

   States0 = maps:fold(S, [], States),

   VF = fun(S0) ->
            #{ "is_end" := End,
               "is_start" := Start,
               "class" := Module,
               "package" := Space} = S0,

            S1 = S0#{
               "fsm_name" => Workflow#workflow.process_name,
               "fsm_package" => Workflow#workflow.space,
               "fsm_class" => Workflow#workflow.module
            },
            case not (End or Start) of
               true ->
                  R0 = bbmustache:compile(VCTemplate, S1),
                  store_translation(Space, Module, OutPath, R0),
                  R1 = bbmustache:compile(VBCTemplate, S1),
                  store_translation(Space, <<"BaseVerticle">>, OutPath, R1);
               false ->
                  ok
            end
         end,   

   lists:foreach(VF, States0),

   % GetClass = fun(Data, Render) -> 
   %             K = Render(Data),
   %             A = maps:get(K, States0),
   %             ?LOG_INFO("Template Return --> ~p", [A#action.module]),
   %             binary_to_list(A#action.module)
   %          end,

   Context = #{ 
      "fsm_name" => Workflow#workflow.process_name,
      "fsm_package" => Workflow#workflow.space,
      "fsm_class" => Workflow#workflow.module,
      "states" => States0},
      %"get_class" => GetClass},

   Result = bbmustache:compile(FSMTemplate, Context),
   store_translation(Workflow#workflow.space, Workflow#workflow.module, OutPath, Result).

   %Package = rosetta_utils:replace(Workflow#workflow.space, [<<"\\.">>], <<"/">>),

%% =============================================================================
%% Internal functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc load mustache template
%% @end
%% @private
%% -----------------------------------------------------------------------------
get_template(Filename) ->
   case file:read_file(Filename) of
      {ok, Body} ->
         bbmustache:parse_binary(Body);
      {error, Reason} ->
         throw(Reason)
   end.

%% -----------------------------------------------------------------------------
%% @doc identifier of end
%% @end
%% @private
%% -----------------------------------------------------------------------------
is_end(<<"end">>) -> true;
is_end(_V) -> false.

%% -----------------------------------------------------------------------------
%% @doc identifier of start
%% @end
%% @private
%% -----------------------------------------------------------------------------
is_start(<<"start">>) -> true;
is_start(_V) -> false.

%% -----------------------------------------------------------------------------
%% @doc make folders
%% @end
%% @private
%% -----------------------------------------------------------------------------
mk_dir(Path) ->
   case filelib:is_dir(Path) of
      false ->
         case file:make_dir(Path) of
            ok ->
               ?LOG_INFO("Created Path  --> ~p", [Path]);
            {error, Reason} ->
               ?LOG_ERROR("ERROR  --> ~p", [Reason])
            end;
      _ -> 
         ?LOG_INFO("Was created Path --> ~p", [Path])
   end.

%% -----------------------------------------------------------------------------
%% @doc make file
%% @end
%% @private
%% -----------------------------------------------------------------------------
store_translation(Space, Module, OutPath, Result) ->
   PList = binary:split(Space, [<<".">>], [global]),
   
   F0 = fun(P, OutPath0) ->
            OutPath1 = list_to_binary([OutPath0, P, <<"/">>]),
            mk_dir(OutPath1),
            OutPath1
         end,
   Path =lists:foldl(F0, OutPath, PList),
   
   OutFile = list_to_binary([Path, Module, <<".java">>]),

   ?LOG_INFO("Output File  --> ~p", [OutFile]),
   {ok, File} = file:open(OutFile, [write]),
   io:format(File, "~s~n", [Result]).