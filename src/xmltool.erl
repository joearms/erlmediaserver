%% Author: ulfangermann
%% Created: Apr 26, 2010
%% Description: TODO: Add description to xmltool
-module(xmltool).
%%
%% Include files
%%
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([get_value/2, parse/1, get_attribute/2, add_url_base/2]).
%%
%% API Functions
%%
get_value(XPath, Xml) ->
	case xmerl_xpath:string(XPath, Xml) of
		[#xmlText{value = Value}] -> Value;
		Other -> Other
	end.

get_attribute(XPath, Node) ->
	case xmerl_xpath:string(XPath, Node) of
		[#xmlAttribute{value = Value}] -> Value;
		O -> O
	end.


parse(Bin) when is_binary(Bin) ->
	{Xml,_} = xmerl_scan:string(binary_to_list(Bin)),
	Xml;

parse(FileName) ->
	{Xml,_} = xmerl_scan:file(FileName),
	Xml.

add_url_base(Xml, NewBaseUrl) ->
	%%error_logger:info_msg("Eingang : ~p~n", [Xml]),
	[UrlBaseNode] = xmerl_xpath:string("//URLBase", Xml),
	[Content] = UrlBaseNode#xmlElement.content,
	create_url_base(UrlBaseNode, Content, NewBaseUrl, Xml).

create_url_base(UrlBaseNode, Content, Value, Xml) ->
	%%error_logger:info_msg("Content ~p~n", [UrlBaseNode]),
	NewUrlBaseNode = UrlBaseNode#xmlElement{content=Content#xmlText{value=Value}},
	error_logger:info_msg("NewContent ~p~n", [NewUrlBaseNode]),
	Parents = UrlBaseNode#xmlElement.parents,
	change_parent(Parents, NewUrlBaseNode, Xml).

change_parent([{Parent, _}|Parents], NewXmlElement, Xml) ->
	%%error_logger:info_msg("~p~n", [Parent]),
	[ParentNode] = xmerl_xpath:string("//" ++ atom_to_list(Parent), Xml),
	%%
	%% loesche das alte element aus der liste (content)
	%%
	error_logger:info_msg("~p~n", [ParentNode#xmlElement.content]),
	
	NewParentNode = ParentNode#xmlElement{parents=[NewXmlElement]},
	change_parent(Parents, NewParentNode, Xml);

change_parent([], NewXmlElement, Xml) ->
	%%error_logger:info_msg("no parents found~n", [NewXmlElement]),
	NewXmlElement.

%%
%% Local Functions
%%

%%
%% Test Functions
%%
add_url_base_test() ->
	Xml = parse("test/ERL_PMS_Test.xml"),
	error_logger:info_msg("Original ~p", [cds_to_xml:record_to_xml(Xml)]),
	error_logger:info_msg("Changed  ~p", [cds_to_xml:record_to_xml(add_url_base(Xml, "http://localhost:9000"))]),
	?assertEqual(5, erlang:length(Xml#xmlElement.content)),
	?assertEqual(5, erlang:length((add_url_base(Xml, "http://localhost:9000"))#xmlElement.content)).
	

get_value_test() ->
	%%Xml = parse("test/browseRequest.xml"),
	Xml = parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">\t<s:Body>\t\t<u:Browse xmlns:u=\"urn:schemas-upnp-org:service:ContentDirectory:1\">\t\t\t<ObjectID>0</ObjectID>\t\t\t<BrowseFlag>BrowseDirectChildren</BrowseFlag>\t\t\t<Filter>*</Filter>\t\t\t<StartingIndex>0</StartingIndex>\t\t\t<RequestedCount>250</RequestedCount>\t\t\t<SortCriteria>d</SortCriteria>\t\t</u:Browse>\t</s:Body></s:Envelope>">>),
	?assertEqual("0", get_value("/s:Envelope/s:Body/u:Browse/ObjectID/text()", Xml)),
	?assertEqual("BrowseDirectChildren", get_value("/s:Envelope/s:Body/u:Browse/BrowseFlag/text()", Xml)),
	?assertEqual("*", get_value("/s:Envelope/s:Body/u:Browse/Filter/text()", Xml)),
	?assertEqual("0", get_value("/s:Envelope/s:Body/u:Browse/StartingIndex/text()", Xml)),
	?assertEqual("250", get_value("/s:Envelope/s:Body/u:Browse/RequestedCount/text()", Xml)),
	?assertEqual("d", get_value("/s:Envelope/s:Body/u:Browse/SortCriteria/text()", Xml)).
			  
get_attribute_test() ->
	Xml = parse("test/browseRequest.xml"),
	?assertEqual("urn:schemas-upnp-org:service:ContentDirectory:1", get_attribute("/s:Envelope/s:Body/u:Browse/@xmlns:u", Xml)).

parse_test() ->
	?assertEqual(error, parse("test/notExistingFile.xml")),
	XmlElement = parse("test/browseRequest.xml"),
	?assertEqual('s:Envelope', XmlElement#xmlElement.name).
	
	