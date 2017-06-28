%%% ---------------------------------------------------------------------------
%%% @author Terry Buhl
%%%
%%% @doc
%%% <p>
%%% Display cross-calls between different applications on the same node.
%%% Calls using rpc:call and rpc:multicall won't be displayed.
%%% </p>
%%%
%%% <p>
%%% Usage:
%%% </p>
%%%
%%% <p><code>
%%% xref_analyze internal_appcalls Beam_modules --opts="opt=val,..."
%%% </code></p>
%%% @end
%%% ---------------------------------------------------------------------------

-module(xref_cmd_internal_appcalls).

-include("../include/xref_analyze.hrl").

-behaviour(xref_gen_cmd).

-type callgraph_el()   :: {mfa(), list()}.

-export([command_name/0,
         execute/1,
         usage/0]).

%%% ---------------------------------------------------------------------------
%%% API
%%% ---------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Return the name of the command the module implements.
%% @end
%%------------------------------------------------------------------------------
-spec command_name() ->
    string().
command_name() ->
    "internal_appcalls".

%%------------------------------------------------------------------------------
%% @doc
%% Execute command
%% @end
%%------------------------------------------------------------------------------
-spec execute(list(string())) ->
    ok.
execute(Parameters) ->
    {ModNames, Opts, Entries} = xref_analyze_lib:parse_args(Parameters, opt_types()),
    DirsAndMods = xref_analyze_lib:get_dirs_and_mods(ModNames),
    Mods = [M || {_, M} <- DirsAndMods],
    ModApps = xref_analyze_lib:get_module_apps(ModNames),
    Apps = lists:usort([D || {_, D} <- ModApps]),
    {ok, Cwd} = file:get_cwd(),
    TempDir = xref_analyze_lib:copy_beams_to_temp(Opts, DirsAndMods),
    [{module, _} = code:load_file(M) || M <- Mods],
    Entries1 = case Entries of
                    [] -> lists:flatten(xref_analyze_lib:get_all_exported(Mods));
                    _ -> Entries
                end,
    analyze("xref_internal_calls", Opts, Mods, Apps, ModApps, Cwd, Entries1),
    xref_analyze_lib:delete_temp_dir(TempDir),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Return command help.
%% @end
%%------------------------------------------------------------------------------
usage() ->
    "Display cross-calls between different applications on the same node.\n"
    "Calls using rpc:call and rpc:multicall won't be displayed.\n"
    "\n"
    "Usage:\n"
    "\n"
    "xref_analyze internal_appcalls Beam_modules --opts=\"opt=val,...\"\n".

%%% ---------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------
-spec opt_types() ->
    list({option_key(), option_type()}).
opt_types() ->
    [{generate_clusters, boolean},
     {temp_dir, string}].


-spec analyze(
    OutputFile :: string(),
    Opts :: options(),
    Mods :: list(module()),
    Apps :: list(atom()),
    ModApps :: list({module(), atom()}),
    Dir :: string(),
    Entries :: list(mfa())
) -> ok.
analyze(_OutputFile, Opts, Mods, _Apps, ModApps, _Dir, Entries) ->
    io:format("~p ~p ModApps: '~p' ~n", [?MODULE, ?LINE, ModApps]),
    xref:start(s),
    xref:set_default(s, [{verbose, false}, {warnings, false}]),
    [xref:add_module(s, M) || M <- Mods],
    CallGraphs = lists:flatten([walk_call_graph(Entry, Mods, Opts, []) || Entry <- Entries]),
    CrossCalls = find_cross_calls(undefined, CallGraphs, ModApps, []),
    io:format("~p ~p CrossCalls: '~p' ~n", [?MODULE, ?LINE, CrossCalls]),
    print_cross_calls(CrossCalls),
    ok.

-spec walk_call_graph(mfa(), list(module()), options(), list(mfa())) ->
    list(callgraph_el()).
walk_call_graph({_, module_info, _}, _Modules, _Opts, _CallPath) ->
    [];
walk_call_graph(MFA, Modules, Opts, CallPath) ->
    {ok, Functions} = xref:analyze(s, {call, MFA}),
    Res =
        lists:foldl(fun({M, _F, _A} = Function, Acc) ->
                case lists:member(M, Modules) of
                    true ->
                        case lists:member(Function, CallPath) of
                            false ->
                                [{MFA, walk_call_graph(Function, Modules, Opts, [Function | CallPath])} | Acc];
                            true ->
                                Acc
                        end;
                    false ->
                        Acc
                end
            end, [], Functions),
    Res.

-spec find_cross_calls(undefined | callgraph_el(), list(callgraph_el()), list({module(), atom()}), list(mfa())) ->
    list(mfa()).
find_cross_calls(_Prev, [], _ModApps, Res) ->
    Res;
find_cross_calls(Prev, [{MFA, Children} | T], ModApps, Res) ->
    io:format("~p ~p MFA: '~p' ~n", [?MODULE, ?LINE, MFA]),
    io:format("~p ~p Children: '~p' ~n", [?MODULE, ?LINE, Children]),
    ChildCrossCalls = find_cross_calls(MFA, Children, ModApps, []),
    case is_cross_call(Prev, MFA, ModApps) of
        true ->
            find_cross_calls(Prev, T, ModApps, [{Prev, MFA}] ++ ChildCrossCalls ++ Res);
        false ->
            find_cross_calls(Prev, T, ModApps, ChildCrossCalls ++ Res)
    end.

-spec is_cross_call(undefined | mfa(), mfa(), list({module(), atom()})) ->
    boolean().
is_cross_call(undefined, _Child, _ModApps) ->
    false;
is_cross_call({Parent, _, _}, {Child, _, _}, ModApps) ->
    find_app_of_mod(Parent, ModApps),
    find_app_of_mod(Child, ModApps),
    case {find_app_of_mod(Parent, ModApps), find_app_of_mod(Child, ModApps)} of
        {{_, ParentApp}, {_, ChildApp}} when ParentApp =/= ChildApp -> true;
        _Other ->
            false
    end.

-spec find_app_of_mod(module(), list({module(), atom()})) ->
    false | {module(), atom()}.
find_app_of_mod(Mod, ModApps) ->
    lists:keyfind(Mod, 1, ModApps).

-spec print_cross_calls(list({mfa(), mfa()})) ->
    ok.
print_cross_calls(Calls) ->
    lists:map(fun({Parent, Child}) ->
            io:format("~45s  ==> ~45s\n",
                      [xref_analyze_lib:fmt_mfa(Parent), xref_analyze_lib:fmt_mfa(Child)])
        end, Calls).
