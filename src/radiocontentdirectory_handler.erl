%% Author: ulfangermann
%% Created: Apr 7, 2010
%% Description: TODO: Add description to radiocontendirectory_handler
-module(radiocontentdirectory_handler).
		
%%
%% Include files
%%
-include_lib("xmerl/include/xmerl.hrl").
-include("/usr/local/lib/yaws/include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(xmltool, [get_value/2, parse/1]).
-import(handler, [make_response/2, get_soapaction/1, get_path/1, get_soapaction/1]).
%%
%% Exported Functions
%%
-export([out/1, handle_request/4]).
out(Arg) ->
    Req = Arg#arg.req,
	error_logger:info_msg("~p~n", [Req]),
    ReqPath = get_path(Req),
	case get_soapaction(Arg#arg.headers) of
		{ok, []} -> SoapAction = [];
		{ok, [SoapAction]} -> error_logger:info_msg("SoapAction : ", [SoapAction])
	end,
    handle_request(Req#http_request.method, ReqPath, SoapAction, Arg).


handle_request('POST', "/service/radiocontentdirectory/control", "urn:schemas-upnp-org:service:ContentDirectory:1#Browse", Arg) ->
	Xml = parse(Arg#arg.clidata),
	error_logger:info_msg("auf geht's ~p~n", [Arg#arg.clidata]),
	ObjectID = get_value("/s:Envelope/s:Body/u:Browse/ObjectID/text()", Xml),
	BrowseFlag = get_value("/s:Envelope/s:Body/u:Browse/BrowseFlag/text()", Xml),
	Filter = get_value("/s:Envelope/s:Body/u:Browse/Filter/text()", Xml),
	StartingIndex = get_value("/s:Envelope/s:Body/u:Browse/StartingIndex/text()", Xml),
	RequestedCount = get_value("/s:Envelope/s:Body/u:Browse/RequestedCount/text()", Xml),
	SortCriteria = get_value("/s:Envelope/s:Body/u:Browse/SortCriteria/text()", Xml),
	BrowseArgs = argument_factory:create_browseArguments(ObjectID, BrowseFlag, Filter, StartingIndex, RequestedCount, SortCriteria),
	BrowseResponse = radiocontentdirectory:browse(BrowseArgs),
	make_response(200, BrowseResponse);

handle_request('GET', "/service/radiocontentdirectory", [], _Arg) ->
	{Element, _Rest} = xmerl_scan:file("docroot/UPnP_AV_ContentDirectory_1.0.xml"),
	Result = lists:flatten(xmerl:export_simple([Element], xmerl_xml)),
	make_response(200, Result);

handle_request(Method, UrlPart, SoapAction, Arg) ->
	io:format("Method : ~p URLPART : ~p SoapAction : ~p Arg ~p~n ", [Method, UrlPart, SoapAction, Arg]).

get_value_test() ->
	Xml = parse("test/browseRequest.xml"),
	%%io:format("Format~p", [Xml]),
	?assertEqual("2", get_value("/s:Envelope/s:Body/u:Browse/ObjectID/text()", Xml)),
	?assertEqual("BrowseDirectChildren", get_value("/s:Envelope/s:Body/u:Browse/BrowseFlag/text()", Xml)),
	?assertEqual("*", get_value("/s:Envelope/s:Body/u:Browse/Filter/text()", Xml)),
	?assertEqual("0", get_value("/s:Envelope/s:Body/u:Browse/StartingIndex/text()", Xml)),
	?assertEqual("1000", get_value("/s:Envelope/s:Body/u:Browse/RequestedCount/text()", Xml)),
	?assertEqual("", get_value("/s:Envelope/s:Body/u:Browse/SortCriteria/text()", Xml)).

