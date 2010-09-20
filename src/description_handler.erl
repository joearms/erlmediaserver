%% Author: ua
%% Created: Feb 8, 2010
%% Description: TODO: Add description to description_handler
-module(description_handler).

-export([out/1, handle_request/3, get_path/1, make_response/2]).

-include("/usr/local/lib/yaws/include/yaws_api.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    ReqPath = get_path(Req),
    handle_request(Req#http_request.method, ReqPath, Arg).

get_path(Req) ->
	{_, Path} = Req#http_request.path, 
	Path.


handle_request('GET', "/description/fetch", _Arg) ->
	error_logger:info_msg("description_handler:handle_request 'GET' /description/fetch"),
	{ok, Element, _Rest} = xmerl_sax_parser:file("docroot/ERL_PMS.xml", [{event_fun, fun sax:parser/3}, {event_state, sax:new_state()}]),
	Result = lists:flatten(xmerl:export_simple(Element, xmerl_xml)),
	error_logger:info_msg("description_handler:handle_request 'GET' /description/fetch Result : " , [Result]),
	make_response(200, Result);

handle_request('GET', "/images/erlmedia.jpg", _Arg) ->
	error_logger:info_msg("description_handler:handle_request 'GET' /images/erlmedia.jpg"),
	{ok, IoDevice} = file:read_file("images/erlmedia.jpg"),
	make_response(200, "image/jpeg", IoDevice);
	

handle_request(Method, UrlPart, Arg) ->
	error_logger:info_msg("Method : ~p URLPART : ~p Arg ~p~n ", [Method, UrlPart, Arg]).


make_response(Status, Message) ->
    make_response(Status, "text/xml", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].


		
