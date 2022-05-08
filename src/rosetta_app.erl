%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%%%-------------------------------------------------------------------

-module(rosetta_app).

-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-define(SOURCE_PATH, <<"/Users/bismark/Work/xentelar/smart-next/src/rsi/docs/">>).
-define(TEMPLATE_PATH, <<"/Users/bismark/src/dev-utilities/roseta-code/fsm/">>).
-define(OUT_PATH,
        <<"/Users/bismark/Work/xentelar/smart-next/src/rsi/app/src/test/java/">>).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
   ?LOG_INFO("Start Rosetta", []),

   Files = list_files(?SOURCE_PATH, <<".puml">>),
   F = fun(File) ->
         Workflow = rosetta_processor:process(File),
         ?LOG_INFO("WORKFLOW RESULT --> ~p", [Workflow]),
         rosetta_model_builder:build(?TEMPLATE_PATH, ?OUT_PATH, Workflow),
         true
      end,
   lists:all(F, Files),

   rosetta_sup:start_link().

stop(_State) ->
   ok.

%% internal functions

list_files(Path, FileExtention) ->
   case filelib:is_file(Path) of
      true ->
         case filelib:is_dir(Path) of
            true ->
               {ok, Files} = file:list_dir(Path),
               filter(Path, Files, FileExtention);
            false ->
               {error, enotdir}
         end;
      false ->
         {error, enoent}
   end.

filter(Path, Files, FileExtention) ->
   F = fun(V, Aux) ->
         ?LOG_DEBUG("EVALUATE FILE --> ~p", [V]),
         case string:find(V, FileExtention, leading) of
            nomatch ->
               Aux;
            _ ->
               File = list_to_binary([Path, V]),
               Aux0 = Aux ++ [File],
               ?LOG_DEBUG("FILE IS ADDING --> ~p", [Aux0]),
               Aux0
         end
      end,
   lists:foldl(F, [], Files).