
%% =============================================================================
%%  rosetta.erl -
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
-module(rosetta).

-include_lib("kernel/include/logger.hrl").

-export([main/1]).

%% =============================================================================
%% API functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%% -----------------------------------------------------------------------------
main(Args) ->

	io:format("Start Rosetta~n", []),
	%?LOG_INFO("Start Rosetta", []),

	process(),
	erlang:halt(0).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc process .puml files
%% @end
%% @private
%% -----------------------------------------------------------------------------
process() ->
	SrcPath = os:getenv("SOURCE_PATH"),
	TplPath = os:getenv("TEMPLATES_PATH"),
	OutPath = os:getenv("OUT_PATH"),

	Files = list_files(SrcPath, ".puml"),
	io:format("Files to process --> ~p~n", [Files]),
	F = fun(File) ->
				Workflow = rosetta_processor:process(File),
				io:format("WORKFLOW RESULT --> ~p~n", [Workflow]),
				rosetta_model_builder:build(TplPath, OutPath, Workflow),
				true
		end,
	lists:all(F, Files).

%% -----------------------------------------------------------------------------
%% @doc list required files on path
%% @end
%% @private
%% -----------------------------------------------------------------------------
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

%% -----------------------------------------------------------------------------
%% @doc filter required files from other files
%% @end
%% @private
%% -----------------------------------------------------------------------------
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
