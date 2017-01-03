%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Basho Technologies, Inc.
%% Copyright (c) 2015 Oleg Tsarev.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(rebar3_neotoma_plugin).
%
% For efficiency in production use, we don't have a dependency on rebar
% itself, so the behaviors this module implements aren't always visible.
%
-ifdef(BASHO_CHECK).
-behaviour(provider).
-type rebar_state()     :: rebar_state:t().
-type rebar_app_info()  :: rebar_app_info:t().
-else.
-type rebar_state()     :: tuple().
-type rebar_app_info()  :: tuple().
-endif.

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(State :: rebar_state()) -> {ok, rebar_state()}.
%%
%% @doc Install the provider.
%%
%% Called when rebar3 first boots, before even parsing the arguments
%% or commands to be run. Purely initiates the provider, and nothing
%% else should be done here.
%%
init(State) ->
    Provider = providers:create([
        {name,        ?PROVIDER                 },
        {module,      ?MODULE                   },
        {namespace,   neotoma                   },
        {bare,        false                     },
        {deps,        ?DEPS                     },
        {example,     "rebar3 neotoma compile"  },
        {short_desc,  "compile peg files."      },
        {desc,        "compile peg files."      },
        {opts,        []                        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(State :: rebar_state())
        -> {ok, rebar_state()} | {error, string()} | {error, {module(), any()}}.
%%
%% @doc Run the code for the plugin.
%% The command line arguments are parsed and dependencies have been run.
%%
do(State) ->
    rebar_api:info("Running neotoma...", []),
    case rebar_state:get(State, escript_main_app, undefined) of
        undefined ->
            Dir = rebar_state:dir(State),
            case rebar_app_discover:find_app(Dir, all) of
                {true, AppInfo} ->
                    AllApps = rebar_state:project_apps(State)
                            ++ rebar_state:all_deps(State),
                    case rebar_app_utils:find(
                            rebar_app_info:name(AppInfo), AllApps) of
                        {ok, AppInfo1} ->
                            %% Use the existing app info instead of newly created one
                            run_neotoma(AppInfo1, State);
                        _ ->
                            run_neotoma(AppInfo, State)
                    end,
                        {ok, State};
                _ ->
                    {error, {?MODULE, no_main_app}}
            end;
        _Name ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
%%
%% @doc Return `Reason' as a string.
%%
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================

-spec run_neotoma(App :: rebar_app_info(), State :: rebar_state())
        -> {ok, rebar_state()} | {error, string()} | {error, {module(), any()}}.

run_neotoma(App, State) ->
    rebar_api:debug("run_neotoma(~p)", [rebar_app_info:name(App)]),
    Source = rebar_app_info:source(App),
    Dir = rebar_state:dir(State),
    rebar_api:debug("source=~p dir=~p", [Source, Dir]),
    case rebar_base_compiler:run(
            State, [], Source, "peg", Source, "erl",
            fun compile_peg/3, [{check_last_mod, false}]) of
        ok ->
            {ok, State};
        Error ->
            Error
    end.

-spec compile_peg(
        Source  :: file:name_all(),
        Target  :: file:name_all(),
        State   :: rebar_state())
        -> ok | skipped | [{error, term()} | {source, file:name_all()}].

compile_peg(Source, Target, _State) ->
    rebar_api:debug("compile_peg(~p, ~p)", [Source, Target]),
    case filelib:last_modified(Target) < filelib:last_modified(Source) of
        true ->
            case neotoma:file(Source, [{output, filename:dirname(Target)}]) of
                ok ->
                    ok;
                {error, _} = Err ->
                    [Err, {source, Source}]
            end;
        _ ->
            skipped
    end.
