%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%%%-------------------------------------------------------------------

-module(rosetta_app).

-include_lib("kernel/include/logger.hrl").

-behaviour(application).

%-define(SOURCE_PATH, <<"/Users/bismark/Work/xentelar/smart-next/src/panic-button/docs/">>).
%-define(TEMPLATE_PATH, <<"/Users/bismark/src/dev-utilities/roseta-code/fsm/">>).
%-define(OUT_PATH, <<"/Users/bismark/Work/xentelar/smart-next/src/panic-button/app/src/test/java/">>).

-export([start/2, stop/1]).

%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%%%-------------------------------------------------------------------
start(_StartType, _StartArgs) ->
   io:format("Start Rosetta~n", []),
   %?LOG_INFO("Start Rosetta", []),

   %ets:new(params, [set, protected]),

   %get_src_path(),
   %get_tpl_path(),
   %get_out_path(),

   %exit(kill).
   rosetta_sup:start_link(),
   %menu().
   process(),
   exit(kill).

%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%%%-------------------------------------------------------------------
stop(_State) ->
   ok.

%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%% @private
%%%-------------------------------------------------------------------
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

%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%% @private
%%%-------------------------------------------------------------------
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

%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%% @private
%%%-------------------------------------------------------------------
menu() ->
   io:format("p=reprocess, r=reset settings, q=quit ~n", []),
   {ok, [Term]} = io:read("--> "),
   
   case Term of
      "p" ->
         process();
      "r" ->
         ok;
      "q" ->
         exit(kill)
   end,

   menu().

%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%% @private
%%%-------------------------------------------------------------------
process() ->

   %[{_, SrcPath}] = ets:lookup(params, src_path),
   %[{_, TplPath}] = ets:lookup(params, tpl_path),
   %[{_, OutPath}] = ets:lookup(params, out_path),
   
   SrcPath = os:getenv("SOURCE_PATH"),
   TplPath = os:getenv("TEMPLATES_PATH"),
   OutPath = os:getenv("OUT_PATH"),

   Files = list_files(SrcPath, ".puml"),
   F = fun(File) ->
         Workflow = rosetta_processor:process(File),
         ?LOG_INFO("WORKFLOW RESULT --> ~p", [Workflow]),
         rosetta_model_builder:build(TplPath, OutPath, Workflow),
         true
      end,
   ?LOG_INFO("Files to process --> ~p", [Files]),
   lists:all(F, Files).

% get_src_path() ->
%    io:format("Enter the path where are the '.puml' files:~n", []),
%    {ok, [Term]} = io:fread("--> ", "~s"),
%    ets:insert(params, {src_path, list_to_binary(Term)}).

% get_tpl_path() ->
%    io:format("Enter the path where are the templates:~n", []),
%    {ok, [Term]} = io:fread("--> ", "~s"),
%    ets:insert(params, {tpl_path, list_to_binary(Term)}).

% get_out_path() ->
%    io:format("Enter the path where are going to generate the code:~n", []),
%    {ok, [Term]} = io:fread("--> ", "~s"),
%    ets:insert(params, {out_path, list_to_binary(Term)}).