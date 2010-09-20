%% Author: ulfangermann
%% Created: May 3, 2010
%% Description: TODO: Add description to handler
-module(handler).
%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("/usr/local/lib/yaws/include/yaws_api.hrl").
%%
%% Exported Functions
%%
-export([make_response/2, make_response/3, make_header/1, make_all_response/3]).
-export([get_soapaction/1, get_path/1]).
%%
%% API Functions
%%
make_response(Status, Message) ->
    make_response(Status, "text/xml", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].
	
get_soapaction(Headers) ->
	{ok, [string:strip(X, both, $") || {_, _, "Soapaction" , _, X} <- Headers#headers.other]}.
	
get_path(Req) ->
	{_, Path} = Req#http_request.path, 
	Path.
%%
%% Local Functions
%%
%%
%% Test Functions
%%
get_soapaction_with_one_test() ->
	A =[{http_header,0,"Soapaction",undefined, "\"urn:schemas-upnp-org:service:ContentDirectory:1#Browse\""}],
	Headers = #headers{other=A},
	?assertEqual({ok, ["urn:schemas-upnp-org:service:ContentDirectory:1#Browse"]}, get_soapaction(Headers)),
	?assertEqual({ok,[]}, get_soapaction(#headers{})).
	

get_soapaction_with_more_than_one_test() ->
	A = [{http_header,10,'Accept-Encoding',undefined,"gzip, deflate"},
		{http_header,11,'Accept-Language',undefined,"de-de"},
		{http_header,0,"Soapaction",undefined, "\"urn:schemas-upnp-org:service:ContentDirectory:1#Browse\""}],
	
	Headers = #headers{other=A},
	?assertEqual({ok, ["urn:schemas-upnp-org:service:ContentDirectory:1#Browse"]}, get_soapaction(Headers)).

