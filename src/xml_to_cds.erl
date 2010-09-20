%% Author: ulfangermann
%% Created: Apr 14, 2010
%% Description: TODO: Add description to xml_to_cds
-module(xml_to_cds).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("../include/upnp.hrl").
-import(xmltool, [get_value/2, get_attribute/2]).
%%
%% Exported Functions
%%
-compile(export_all).
-export([]).

%%
%% API Functions
%%
createCDSObject(FileName) ->
	Xml = parse(FileName),	
	Class = getClass(Xml),
	%%io:format("Class : ~p~n", [Class]),
	createCDSObject(Class, Xml).
	
createCDSObject(Class, Xml) when Class == "object.item.audioItem.audioBroadcast" ->
	Region = get_value("item/upnp:region/text()", Xml),
	RadioCallSign = get_value("item/upnp:radioCallSign/text()", Xml),
	RadionStationId = get_value("item/upnp:radioStationID/text()", Xml),
	RadioBand = get_value("item/upnp:radioBand/text()", Xml),
	ChannelNr = get_value("item/upnp:channelNr/text()", Xml),
	#cdsaudiobroadcast{cdsaudioitem = createCDSObject("object.item.audioItem", Xml),
					   region=Region, radioCallSign=RadioCallSign, 
					   radioStationId = RadionStationId, radioBand = RadioBand,
					   channelNr = ChannelNr};
	
createCDSObject(Class, Xml) when Class == "object.item.audioItem" ->
	Description = get_value("item/dc:description/text()", Xml),	
	LongDescription = get_value("item/upnp:longDescription/text()", Xml),
	Genre = get_value("item/upnp:genre/text()", Xml),
	Publisher = get_value("item/dc:publisher/text()", Xml),
	Language = get_value("item/dc:language/text()", Xml),
	Relation = get_value("item/dc:relation/text()", Xml),
	Rights = get_value("item/dc:rights/text()", Xml),
	#cdsaudioitem{cdsitem = createCDSObject("object.item", Xml),
				  description = Description, longDescription = LongDescription, genre = Genre,
				  publisher = Publisher, language = Language, relation = Relation, rights = Rights};

createCDSObject(Class, Xml) when Class == "object.item" ->
	#cdsitem{cdsobject = createCDSObject("object", Xml), refId = "-1"};

createCDSObject(Class, Xml) when Class == "object" ->
	Creator = get_value("item/dc:creator/text()", Xml),
	#cdsobject{creator=Creator, resourceList=createCDSResources(Xml)}.	

createCDSResources(Xml) ->
	Resources  = xmerl_xpath:string("item/res", Xml),
	lists:foldl(fun(Node, CDSResources) -> [createCDSResource(Node) | CDSResources] end,[], Resources).	

createCDSResource(Node) ->
	Size = get_attribute("/item/res/@size", Node), 
	Duration=get_attribute("/res/@duration",Node), 
	BitRate=get_attribute("/res/@bitrate",Node), 
	SampleFreq=get_attribute("/res/@sampleFreq",Node), 
	BitsPerSample=get_attribute("/res/@bitsPerSample",Node), 
	NrAudioChannels=get_attribute("/res/@nrAudioChannels",Node),
	Resolution=get_attribute("/res/@resolution",Node),
	ColorDepth=get_attribute("/res/@colorDepth",Node),
	ProtocolInfo=get_attribute("/res/@protocolInfo",Node), 
	Protection=get_attribute("/res/@protection",Node),
	ImportURI=get_attribute("/res/@importURI",Node),
	Name  = get_value("/res/text()", Node),
	#cdsresource{name=Name, size=Size, duration=Duration, bitRate=BitRate, sampleFreq=SampleFreq,
				 bitsPerSample=BitsPerSample, nrAudioChannels=NrAudioChannels, 
				 resolution=Resolution, colorDepth=ColorDepth, protocolInfo=ProtocolInfo,
				 protection=Protection, importURI=ImportURI}.

  	
getClass(Xml) ->	
	get_value("item/upnp:class/text()", Xml).

parse(FileName) ->
	{Xml,_} = xmerl_scan:file(FileName),
	Xml.

	
%%
%% Local Functions
%%

%%
%% Test Functions
%%
parse_test() ->
	parse("radiodb/AllStations/WDET.xml").
