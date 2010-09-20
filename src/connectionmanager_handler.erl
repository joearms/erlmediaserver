%% Author: ua
%% Created: Apr 30, 2010
%% Description: TODO: Add description to connectionmanager_handler
-module(connectionmanager_handler).
-export([out/1, handle_request/3, get_path/1, make_response/2]).

-include("/usr/local/lib/yaws/include/yaws_api.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    ReqPath = get_path(Req),
    handle_request(Req#http_request.method, ReqPath, Arg).

get_path(Req) ->
	{_, Path} = Req#http_request.path, 
	Path.

handle_request('GET', "/service/connectionmanager", _Arg) ->
	error_logger:info_msg("'GET', /service/connectionmanager"),
	{Element, _Rest} = xmerl_scan:file("docroot/UPnP_AV_ConnectionManager_1.0.xml"),
	Result = lists:flatten(xmerl:export_simple([Element], xmerl_xml)),
	%%error_logger:info_msg("'GET', /service/connectionmanager Result", [Result]),
	make_response(200, Result);

handle_request('GET', "/service/connectionmanager/control", _Arg) ->
	error_logger:info_msg("'GET', /service/connectionmanager/control"),
	make_response(200, []);

handle_request(Method, UrlPart, Arg) ->
	io:format("Method : ~p URLPART : ~p Arg ~p~n ", [Method, UrlPart, Arg]).

make_response(Status, Message) ->
    make_response(Status, "text/xml", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].
