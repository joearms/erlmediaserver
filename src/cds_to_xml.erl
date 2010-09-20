%% Author: ulfangermann
%% Created: Mar 2, 2010
%% Description: TODO: Add description to xmltool
-module(cds_to_xml).
-author('uangermann@googlemail.com').
-vsn(1.0).

%%
%% Include files
%%
-include("../include/upnp.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").


%%
%% Exported Functions
%%
-compile([export_all]).
%%-export([test/0]).
%%-export([elementsToXML/2]).

%%
%% API Functions
%%

toXml(CDSType) ->
	Data = getObjectXMLElementName(CDSType, attributesForXml(CDSType)),
	record_to_xml(elementsToXML(CDSType, Data)).

getObjectXMLElementName(CDSType, Attributes) when is_record(CDSType, cdsobject) ->
	[{CDSType#cdsobject.xmlElementName, Attributes,[]}].

attributesForXml(CDSType) when is_record(CDSType, cdsobject) ->
	[{'id', CDSType#cdsobject.id},
	 {'parentID', CDSType#cdsobject.parentId},
	 {'restricted', CDSType#cdsobject.restricted}];

attributesForXml(CDSType) when is_record(CDSType, cdsitem) ->
	[{'refId', CDSType#cdsitem.refId}] ++ attributesForXml(CDSType#cdsitem.cdsobject).

elementsToXML(CDSType, Acc) when is_record(CDSType, cdsstorageFolder) ->	
	Acc ++ [
			{'upnp:storageUsed', [], [CDSType#cdsstorageFolder.storageUsed]}
		   ]
	++ elementsToXML(CDSType#cdsstorageFolder.cdscontainer, Acc);

elementsToXML(CDSType, Acc) when is_record(CDSType, cdscontainer) ->
	Acc ++ elementsToXML(CDSType#cdscontainer.cdsobject, Acc)
		++ [{'upnp:createClass', [{includeDerived, false}], [CDSType#cdscontainer.upnpClass]}];

elementsToXML(CDSType, Acc) when is_record(CDSType, cdsobject) ->
	Acc ++ [
	 		{'dc:title', [], [CDSType#cdsobject.title]},
	 		{'upnp:class', [], [CDSType#cdsobject.upnpClass]},
	 		{'dc:creator', [], [CDSType#cdsobject.creator]},	 
	 		{'upnp:writeStatus', [], [CDSType#cdsobject.writeStatus]}
			];

elementsToXML(CDSType, Acc) when is_record(CDSType, cdsalbum) ->
	Acc ++ [
			{'dc:description', [], [CDSType#cdsalbum.description]},
			{'dc:date', [], [CDSType#cdsalbum.date]},
			{'upnp:storageMedium', [], [CDSType#cdsalbum.storageMedium]}
			];

elementsToXML(CDSType, Acc) when is_record(CDSType, cdsmusicalbum) ->
	 Acc ++ [
			 {'upnp:artist', [], [CDSType#cdsmusicalbum.artist]},
			 {'upnp:genre', [], [CDSType#cdsmusicalbum.genre]},
			 {'upnp:producer', [], [CDSType#cdsmusicalbum.producer]}
			 ];

elementsToXML(CDSType, Acc) when is_record(CDSType, cdsaudioitem) ->
	Acc ++ [
			{'dc:description', [], [CDSType#cdsaudioitem.description]},
			{'dc:publisher', [], [CDSType#cdsaudioitem.publisher]},
			{'dc:language', [], [CDSType#cdsaudioitem.language]},
			{'dc:relation', [], [CDSType#cdsaudioitem.relation]},
			{'dc:rights', [], [CDSType#cdsaudioitem.rights]}
			];

elementsToXML(CDSType, Acc) when is_record(CDSType, cdsimageitem) ->
	Acc ++ [
			{'dc:description', [], [CDSType#cdsimageitem.description]},
			{'dc:publisher', [], [CDSType#cdsimageitem.publisher]},
			{'dc:date', [], [CDSType#cdsimageitem.date]},
			{'dc:rights', [], [CDSType#cdsimageitem.rights]},
			{'upnp:longDescription', [], [CDSType#cdsimageitem.longDescription]},
			{'upnp:storageMedium', [], [CDSType#cdsimageitem.storageMedium]},
			{'upnp:rating', [], [CDSType#cdsimageitem.rating]}
			];
			
elementsToXML(CDSType, Acc) when is_record(CDSType, cdsaudiobroadcast) ->
	Acc ++ [
			{'upnp:region', [], [CDSType#cdsaudiobroadcast.region]},
			{'upnp:radioCallSign', [], [CDSType#cdsaudiobroadcast.radioCallSign]},
			{'upnp:radioStationID', [], [CDSType#cdsaudiobroadcast.radioStationId]},
			{'upnp:radioBand', [], [CDSType#cdsaudiobroadcast.radioBand]},
			{'upnp:channelNr', [], [CDSType#cdsaudiobroadcast.channelNr]}
			];

elementsToXML(CDSType, Acc) when is_record(CDSType, cdsphoto) ->
	Acc ++ [].


%%
%% Local Functions
%%

record_to_list(Tag, Record) when is_atom(Tag) ->  
	lists:nthtail(1, tuple_to_list(Record)). 


createXml(Tag, Value) ->
	"<" ++ Tag ++ ">" ++ Value ++ "</" ++ Tag ++">".
	
record_to_xml(Data) ->
	io:format("Data ~p~n", [Data]),	
    {RootElement, _} = xmerl_scan:string(?DIDL_LITE_HEADER),
    #xmlElement{content = Content} = RootElement,
    NewContent = Content ++ lists:flatten([Data]),
    NewRootElement=RootElement#xmlElement{content=NewContent},    
    Export=xmerl:export_simple([NewRootElement], xmerl_xml),
    lists:flatten(Export).

%%
%% Test Funktionen
%%


test1() ->
	A = #cdsobject{},
	record_to_list(cdsobject, A).
		
testGetObjectXMLElementName() ->
	getObjectXMLElementName(#cdsobject{}, attributesForXml(#cdsobject{})).

testToXml() ->
	toXml(#cdsobject{}).

testAttributesForXml() ->
	attributesForXml(#cdsitem{cdsobject=#cdsobject{}}).

testElementsToXML() ->
	record_to_xml(elementsToXML(#cdscontainer{cdsobject=#cdsobject{}}, [])).
