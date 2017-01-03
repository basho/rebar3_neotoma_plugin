%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Basho Technologies, Inc.
%% Copyright (c) 2015 Tristan Sloughter.
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

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(State :: rebar_state()) -> {ok, rebar_state()}.
%%
%% @doc Install the provider.
%%
init(State) ->
    Provider = providers:create([
        {name,        neotoma },
        {module,      ?MODULE },
        {bare,        true },
        {deps,        []},
        {example,     "rebar3 neotoma" },
        {short_desc,  "compile peg files." },
        {desc,        "compile peg files." },
        {opts,        [] }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(State :: rebar_state())
        -> {ok, rebar_state()} | {error, string()} | {error, {module(), any()}}.
%%
%% @doc Run the code for the plugin.
%%
do(State) ->
    rebar_api:info("Running neotoma...", []),
    Apps = case rebar_state:current_app(State) of
        undefined ->
            rebar_state:project_apps(State);
        App ->
            [App]
    end,
    run_neotoma(Apps, State).

-spec format_error(any()) -> iolist().
%%
%% @doc Return `Reason' as a string.
%%
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================

-spec run_neotoma(
        Apps    :: [rebar_app_info()],
        State   :: rebar_state())
        -> {ok, rebar_state()} | {error, string()} | {error, {module(), any()}}.

run_neotoma([App | Apps], State) ->
    rebar_api:debug("run_neotoma: ~s", [rebar_app_info:name(App)]),
    SrcDir = filename:join(rebar_app_info:dir(App), "src"),
    case rebar_base_compiler:run(
            rebar_state:opts(State), [], SrcDir, ".peg", SrcDir, ".erl",
            fun compile_peg/3, [{check_last_mod, true}]) of
        ok ->
            run_neotoma(Apps, State);
        {error, _} = Error ->
            Error
    end;
run_neotoma([], State) ->
    {ok, State}.

-spec compile_peg(
        Source  :: file:name_all(),
        Target  :: file:name_all(),
        Config  :: term())  % Likely a dict(), but we don't care about it
        -> ok | skipped | [{error, term()} | {source, file:name_all()}].

compile_peg(Source, Target, _Config) ->
    rebar_api:info("Compiling ~s...", [filename:basename(Source)]),
    case neotoma:file(Source, [{output, filename:dirname(Target)}]) of
        ok ->
            ok;
        {error, _} = Err ->
            [Err, {source, Source}]
    end.
