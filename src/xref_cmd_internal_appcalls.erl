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


-type callgraph_node() :: {Module :: atom(),
                            Function :: atom(),
                            Arity :: integer(),
                            Application :: atom()}.

-type callgraph_el()   :: {callgraph_node(), list()}.

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
    xref:start(s),
    xref:set_default(s, [{verbose, false}, {warnings, false}]),
    [xref:add_module(s, M) || M <- Mods],
    CallGraphs = lists:flatten(
                   [walk_call_graph(Entry, Mods, Opts, ModApps, []) ||
                    Entry <- Entries]),
    CrossCalls = find_cross_calls(undefined, CallGraphs, []),
    print_cross_calls(CrossCalls),
    ok.

-spec walk_call_graph(Mfa, Modules, Opts, ModApps, CallPath) -> Result when
      Mfa :: mfa(),
      Modules :: list(module()),
      Opts :: options(),
      ModApps :: list({module(), atom()}),
      CallPath :: list(mfa()),
      Result :: list(callgraph_el()).
walk_call_graph({_, module_info, _}, _Modules, _Opts, _ModApps, _CallPath) ->
    [];
walk_call_graph({Mod, Fun, Ar} = MFA, Modules, Opts, ModApps, CallPath) ->
    {ok, Functions} = xref:analyze(s, {call, MFA}),
    Res =
        lists:foldl(fun({M, _F, _A} = Function, Acc) ->
                case lists:member(M, Modules) of
                    true ->
                        case lists:member(Function, CallPath) of
                            false ->
                                App = find_app_of_mod(Mod, ModApps),
                                [{{Mod, Fun, Ar, App}, walk_call_graph(Function, Modules, Opts, ModApps, [Function | CallPath])} | Acc];
                            true ->
                                Acc
                        end;
                    false ->
                        Acc
                end
            end, [], Functions),
    Res.

-spec find_cross_calls(Prev, CallGraphs, ResAcc) -> Result when
      Prev :: undefined | callgraph_node() | callgraph_el(),
      CallGraphs :: list(callgraph_el()),
      ResAcc :: list({callgraph_node(), callgraph_node()}),
      Result :: list({callgraph_node(), callgraph_node()}).
find_cross_calls(_Prev, [], Res) ->
    Res;
find_cross_calls(Prev, [{{_, _, _, ChildApp} = MFA, Children} | T], Res) ->
    ChildCrossCalls = find_cross_calls(MFA, Children, []),
    ParentApp = get_parent_app(Prev),
    case ParentApp =:= ChildApp of
        true ->
            find_cross_calls(Prev, T, [{Prev, MFA}] ++ ChildCrossCalls ++ Res);
        false ->
            find_cross_calls(Prev, T, ChildCrossCalls ++ Res)
    end.

-spec find_app_of_mod(module(), list({module(), atom()})) ->
    false | {module(), atom()}.
find_app_of_mod(Mod, ModApps) ->
    {_, App} = lists:keyfind(Mod, 1, ModApps),
    App.

-spec print_cross_calls(list({callgraph_node(), callgraph_node()})) ->
    ok.
print_cross_calls(Calls) ->
    lists:map(fun({{PMod, PFun, PAr, _}, {CMod, CFun, CAr, _}}) ->
            io:format("~45s  ==> ~45s\n",
                      [xref_analyze_lib:fmt_mfa({PMod, PFun, PAr}), xref_analyze_lib:fmt_mfa({CMod, CFun, CAr})])
        end, lists:usort(Calls)),
    ok.

-spec get_parent_app(Function) -> Result when
      Function :: undefined | callgraph_node(),
      Result :: atom().
get_parent_app({_, _, _, App}) ->
    App;
get_parent_app(undefined) ->
    undefined.
