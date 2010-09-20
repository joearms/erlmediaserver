%% Author: ua
%% Created: May 21, 2010
%% Description: TODO: Add description to dir_walker
-module(dir_walker).

%%
%% Include files
%%
-include("../include/upnp.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-compile(export_all).
-record(state, {subDirs = []}).
%%
%% API Functions
%%

loadFolder(RootFolder) ->
	io:format("/t1...~p~n", [RootFolder]),
	case get_dirs(RootFolder) of 
		SubDirs -> walk_the_tree(RootFolder, SubDirs, new_state());
		[]      -> walk_the_tree(RootFolder,[], new_state())
	end.
	
walk_the_tree(AktDir, [Dir|Rest], State) ->
	io:format("/t/t2...: ~p : ~p~n", [AktDir, Dir]),
	walk_the_tree(AktDir, Rest, State);

walk_the_tree(AktDir,[], State) ->
	io:format("/t/t/t3...~p~n", [AktDir]),
	io:format("Ende").
%%
%% Local Functions
%%
get_abs_dir(Dir) ->
	case file:get_cwd() of
		{ok, AbsDir} -> AbsDir ++ "/" ++ Dir;
		Other -> error_logger:error_msg("can' find ~p~n", [Dir])
	end.

get_dirs(Dir) ->
	lists:filter(fun(X) -> filelib:is_dir(dir_walker:get_abs_dir(Dir ++ "/" ++ X)) and (string:sub_string(filename:basename(X),1,1) /= ".") end,  filelib:wildcard("*", dir_walker:get_abs_dir(Dir))).

new_state() ->
	#state{subDirs=[]}.

%%
%% Test Functions
%%
get_abs_dir_test() ->
	?assertEqual("/Users/ua/projekte/erlang/lilly/erlmediaserver/radiodb", get_abs_dir("radiodb")).

get_dirs_test() ->
	?assertEqual(2, erlang:length(get_dirs("radiodb"))).