%% Author: ua
%% Created: Apr 14, 2010
%% Description: TODO: Add description to cds_lib
-module(cds_lib).

%%
%% Include files
%%
-include("../include/upnp.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
addToDict(CDSObject, Dict) when is_record(CDSObject, cdsstorageFolder)->
	dict:store(?TITLE_FROM_CDSSTORAGEFOLDER(CDSObject), CDSObject, Dict).

searchByTitle(Title, Dict) ->
	dict:find(Title, Dict).

addClient(CDSObject, Child) when is_record(CDSObject, cdsstorageFolder)->
	NewChild = updateCDSObject(Child, ?ID_FROM_CDSSTORAGEFOLDER(CDSObject)),
	NewParent = ?CDSSTORAGEFOLDER1(NewChild, ?CHILDCOUNT_FROM_CDSSTORAGEFOLDER(CDSObject), ?CHILDLIST_FROM_CDSSTORAGEFOLDER(CDSObject)),
	{ok, NewParent, NewChild}.

updateCDSObject(CDSObject, Value) when is_record(CDSObject, cdsstorageFolder) ->
	O = updateCDSObject(?OJECT_FROM_CDSSTORAGEFOLDER(CDSObject), Value),
	CDSObject#cdsstorageFolder{cdscontainer = #cdscontainer{cdsobject = O}};

updateCDSObject(CDSObject, Value) when is_record(CDSObject, cdsobject) ->
	CDSObject#cdsobject{parentId = Value}.

%%
%% Local Functions
%%

%%
%% Test Functions
%%
updateCDSObject_test() ->
	A = ?CDSSTORAGEFOLDER("10", "Child"),
	C = updateCDSObject(A, "20"),
	?assertEqual("20", ((C#cdsstorageFolder.cdscontainer)#cdscontainer.cdsobject)#cdsobject.parentId),
	?assertEqual("Child", ((C#cdsstorageFolder.cdscontainer)#cdscontainer.cdsobject)#cdsobject.title).

updateCDSObject1_test() ->
	O = #cdsobject{parentId = "0", title="Object"},
	P = updateCDSObject(O, "123"),
	?assertEqual("123", P#cdsobject.parentId),
	?assertEqual("Object", P#cdsobject.title).

addClient_test() ->
	A = ?CDSSTORAGEFOLDER("10", "Parent"),
	B = ?CDSSTORAGEFOLDER("20", "Child"),
	{ok, C, D} = addClient(A,B),
	?assertEqual(1, (C#cdsstorageFolder.cdscontainer)#cdscontainer.childCount),
	?assertEqual(1, length((C#cdsstorageFolder.cdscontainer)#cdscontainer.childList)),	
	?assertEqual("10", ((D#cdsstorageFolder.cdscontainer)#cdscontainer.cdsobject)#cdsobject.parentId).
	
addToDict_test() ->
	D1 = dict:new(),
	A = ?CDSSTORAGEFOLDER("10", "Tester"),
	D2 = addToDict(A, D1),
	{ok, Value} = searchByTitle("Tester", D2),
	?assertEqual("Tester", ?TITLE_FROM_CDSSTORAGEFOLDER(Value)).

