%% Author: ulfangermann
%% Created: Apr 26, 2010
%% Description: TODO: Add description to argument_factory
-module(argument_factory).

%%
%% Include files
%%
-include("../include/upnp.hrl").
%%
%% Exported Functions
%%
-compile(export_all).
-export([create_browseRequest/6, create_browseResponse/1, create_browseResult/ 3, create_browseArguments/6]).

%%
%% API Functions
%%
create_browseRequest(ObjectID, BrowserFlag, Filter, StartingIdx, RequestedCount, SortCriteria) -> 
	  [{'s:Body', [],
		 [{'u:Browse', [{'xmlns:u',"urn:schemas-upnp-org:service:ContentDirectory:1"}],
		  [{'ObjectID',[],[ObjectID]},
		   {'BrowseFlag',[],[BrowserFlag]},
		   {'Filter',[],[Filter]},
		   {'StartingIndex',[],[StartingIdx]},
		   {'RequestedCount',[],[RequestedCount]},
		   {'SortCriteria',[],[SortCriteria]}
		  ]}
		 ]}
	   ].

create_browseResponse(BrowseResult) when is_record(BrowseResult, browseResult)->
	"not implemented".

create_browseArguments(ObjectID, BrowserFlag, Filter, StartingIdx, RequestedCount, SortCriteria) ->
	#browseArguments{objectID=ObjectID, browserFlag=BrowserFlag, filter=Filter, 
					 startingIndex=StartingIdx, requestedCount=RequestedCount,
					 sortCriteria=SortCriteria}.

create_browseResult(Result, NumberReturned, TotalMatches) ->
	#browseResult{result=Result, numberReturned=NumberReturned, totalMatches=TotalMatches}.

%%
%% Local Functions
%%

%%
%% Test Functions
%%
create_browseRequest1() ->
	create_browseRequest("2", "BrowseDirectChildren", "*", "0", "1000", "").
	