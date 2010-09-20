%% Author: ulfangermann
%% Created: Jan 20, 2010
%% Description: TODO: Add description to stringplus
-module(stringplus).
-author('uangermann@googlemail.com').
-vsn(1.0).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([startsWith/2]).
%%
%% API Functions
%%
startsWith(Original, Substr) ->
	Len = string:len(Substr),
	StartStr = string:substr(Original, 1, Len),
	string:equal(StartStr, Substr).
%%
%% Local Functions
%%	
startsWith_test() ->
	?assertEqual(true, startsWith("M-Search sdsadsadsadsad", "M-Search")),
	?assertEqual(false, startsWith("M-Search sdsadsadsadsad", "Fault")).


		  
