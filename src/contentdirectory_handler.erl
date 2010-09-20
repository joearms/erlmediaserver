%% Author: ua
%% Created: Feb 8, 2010
%% Description: TODO: Add description to description_handler
-module(contentdirectory_handler).

-export([out/1,handle_request/4]).

-include("/usr/local/lib/yaws/include/yaws_api.hrl").
-import(xmltool, [get_value/2, parse/1]).
-import(handler, [make_response/2, get_soapaction/1, get_path/1, get_soapaction/1]).

out(Arg) ->
	error_logger:info_msg("Arg ~p~n",[Arg]),
    Req = Arg#arg.req,
    ReqPath = get_path(Req),
	case get_soapaction(Arg#arg.headers) of
		{ok, []} -> SoapAction = [];
		{ok, [SoapAction]} -> error_logger:info_msg("SoapAction : ", SoapAction)
	end,
    handle_request(Req#http_request.method, ReqPath, SoapAction, Arg).

handle_request('GET', "/service/contentdirectory", [], _Arg) ->
	error_logger:info_msg("'GET', /service/contentdirectory"),
	{Element, _Rest} = xmerl_scan:file("docroot/UPnP_AV_ContentDirectory_1.0.xml"),
	Result = lists:flatten(xmerl:export_simple([Element], xmerl_xml)),
	%%error_logger:info_msg("'GET', /service/contentdirectory Result : ", [Result]),
	make_response(200, Result);

handle_request('POST', "/service/contentdirectory/control", "urn:schemas-upnp-org:service:ContentDirectory:1#Browse", Arg) ->
	error_logger:info_msg("START /service/contentdirectory/control, urn:schemas-upnp-org:service:ContentDirectory:1#Browse"),
	Xml = parse(Arg#arg.clidata),
	ObjectID = get_value("/s:Envelope/s:Body/u:Browse/ObjectID/text()", Xml),
	BrowseFlag = get_value("/s:Envelope/s:Body/u:Browse/BrowseFlag/text()", Xml),
	Filter = get_value("/s:Envelope/s:Body/u:Browse/Filter/text()", Xml),
	StartingIndex = get_value("/s:Envelope/s:Body/u:Browse/StartingIndex/text()", Xml),
	RequestedCount = get_value("/s:Envelope/s:Body/u:Browse/RequestedCount/text()", Xml),
	SortCriteria = get_value("/s:Envelope/s:Body/u:Browse/SortCriteria/text()", Xml),
	BrowseArgs = argument_factory:create_browseArguments(ObjectID, BrowseFlag, Filter, StartingIndex, RequestedCount, SortCriteria),
	BrowseResponse = contentdirectory:browse(BrowseArgs),
	make_response(200, []);

handle_request(Method, UrlPart, SoapAction,  Arg) ->
	io:format("Method : ~p URLPART : ~p SoapAction : ~p Arg ~p~n ", [Method, UrlPart, SoapAction, Arg]).

