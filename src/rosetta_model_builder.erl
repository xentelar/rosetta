
%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%%%-------------------------------------------------------------------

-module(rosetta_model_builder).

-include_lib("kernel/include/logger.hrl").

-include("rosetta_workflow.hrl").

-export([build/3]).

build(SourcePath, OutPath, Workflow) ->
   TemplateName = Workflow#workflow.template,

   TemplateFile = list_to_binary([SourcePath, TemplateName]),

   ?LOG_INFO("Template Name --> ~p", [TemplateFile]),
   Template = get_template(TemplateFile),

   #workflow{states = States} = Workflow,

   S = fun(K, State, Aux) -> 
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

   GetClass = fun(Data, Render) -> 
               K = Render(Data),
               A = maps:get(K, States0),
               ?LOG_INFO("Template Return --> ~p", [A#action.module]),
               binary_to_list(A#action.module)
            end,

   Context = #{ 
      "fsm_name" => Workflow#workflow.process_name,
      "fsm_package" => Workflow#workflow.space,
      "fsm_class" => Workflow#workflow.module,
      "states" => States0,
      "get_class" => GetClass},

   Result = bbmustache:compile(Template, Context),
   %Package = rosetta_utils:replace(Workflow#workflow.space, [<<"\\.">>], <<"/">>),

   PList = binary:split(Workflow#workflow.space, [<<".">>], [global]),
   
   F0 = fun(P, OutPath0) ->
            OutPath1 = list_to_binary([OutPath0, P, <<"/">>]),
            mk_dir(OutPath1),
            OutPath1
         end,
   Path =lists:foldl(F0, OutPath, PList),
   
   OutFile = list_to_binary([Path, Workflow#workflow.module, <<".java">>]),

   ?LOG_INFO("Output File  --> ~p", [OutFile]),
   {ok, File} = file:open(OutFile, [write]),
   io:format(File, "~s~n", [Result]).

get_template(Filename) ->
   case file:read_file(Filename) of
      {ok, Body} ->
         bbmustache:parse_binary(Body);
      {error, Reason} ->
         throw(Reason)
   end.

is_end(<<"end">>) -> true;
is_end(_V) -> false.

is_start(<<"start">>) -> true;
is_start(_V) -> false.

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