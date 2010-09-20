%%
%% SSDP 
%%
-define(MULTICAST_GROUP, {239,255,255,250}).
-define(MULTICAST_PORT, 1900).
-define(RECEIVE_OPTIONS, [{broadcast,true}, {reuseaddr,true},{multicast_loop,false},{multicast_if, os_info:get_ip()},{multicast_ttl, 32}]).
-define(SEND_OPTIONS, [{broadcast,true}, {active, false},{ip, Ip}, {reuseaddr,true},{multicast_loop,true},{multicast_if, os_info:get_ip()},{multicast_ttl, 32}]).


-define(ERLMEDIASERVER_APP_FILE, erlmediaserver).

%% root device
-record(rootdevice, {uuid, wirelessmode, port, descriptionuri="/description/fetch", services=[], os, 
					 ip, rootdevice="upnp:rootdevice", elementname="device"}).

%% Die Felder creator und writeStatus sind optional
%% writeStatus   Valid values are WRITABLE, PROTECTED, NOT_WRITABLE, UNKNOWN, or MIXED

-record(cdsobject, {id, parentId, title, creator="", resourceList=[], restricted=false,
					 writeStatus="UNKNOWN", numericalId = 0, trackNumber = 0, thumbImage,
					upnpClass = "object", xmlElementName = object}).

-record(cdsresource, {name, size=-1, duration, bitRate=-1, sampleFreq=-1, bitsPerSample=-1, 
					  nrAudioChannels=-1,resolution, colorDepth=-1, protocolInfo, protection, importURI}).

-record(cdscontainer, {cdsobject, classpermission, childCount = 0, childList=[], 
					   createClasses=[], searchClasses = [], searchable=true,
					   upnpClass = "object.container", xmlElementName = "container"}).

-record(classpermission, {name, includeDerived=false, friendlyName}).

-record(cdsstorageFolder, {cdscontainer, storageUsed=-1,
						   upnpClass = "object.container.storageFolder"}).

-record(cdsitem, {cdsobject, refId = -1, 
				  upnpClass = "object.item", xmlElementName = "item"}).

-record(cdsalbum, {cdsContainer, storageMedium, description, longDescription,
				   publiser, contributor, date, upnpClass = "object.container.album"}).

-record(cdsmusicalbum, {cdsalbum, artist, genre, producer, albumArtURI, toc}).

-record(cdsaudioitem, {cdsitem, description, publisher, language, relation, rights,
					   longDescription, genre, upnpClass = "object.item.audioItem"}).

-record(cdsimageitem, {cdsitem, longDescription, storageMedium, rating,
					   description, publisher, date, rights, upnpClass = "object.item.imageItem"}).

-record(cdsaudiobroadcast, {cdsaudioitem, upnpClass = "object.item.audioItem.audioBroadcast",
							region, radioCallSign, radioStationId, radioBand, channelNr}).

-record(cdsphoto, {cdsimageitem, upnpClass = "object.item.imageItem.photo", album}).

-record(cdsplaylistcontainer, {cdscontainer, artist, genre, longDescription, producer, storageMedium,
							   description, contributor, date, language, rights}).

-record(result, {container, item}).
-record(searchArguments, {containerID, searchCriteria, filter, startingIndex, requestedCount, sortCriteria}).
-record(searchResult, {result, numberReturned, totalMatches, updateID}).
-record(browseArguments, {objectID, browserFlag, filter, startingIndex, requestedCount, sortCriteria}).
-record(browseResult, {result, numberReturned, totalMatches}).
%%
%% CRUD
%%
-define(ROOTFOLDER, #cdsstorageFolder{cdscontainer=#cdscontainer{cdsobject=#cdsobject{id="0", parentId="-1", title="Root", creator="UNKNOWN"}}}).
-define(CDSSTORAGEFOLDER(Id, Title), cdscontainer=#cdscontainer{cdsobject=#cdsobject{id="0", parentId="-1", title="Root", creator="UNKNOWN"}}).
-define(CDSSTORAGEFOLDER1(Child, ChildCount, ChildList), #cdsstorageFolder{cdscontainer=#cdscontainer{childCount=ChildCount, childList=ChildList}}).
-define(CDSSTORAGEFOLDER(ParentId, Id, Title, ChildList), #cdsstorageFolder{cdscontainer=#cdscontainer{cdsobject=#cdsobject{id=Id, parentId=ParentId, title=Title, creator="UNKNOWN"}, childCount=erlang:length(ChildList), childList = ChildList}}).
%%
%% Getter
%%
-define(OJECT_FROM_CDSSTORAGEFOLDER(X), (X#cdsstorageFolder.cdscontainer)#cdscontainer.cdsobject).
-define(ID_FROM_CDSSTORAGEFOLDER(X), ((X#cdsstorageFolder.cdscontainer)#cdscontainer.cdsobject)#cdsobject.id).
-define(TITLE_FROM_CDSSTORAGEFOLDER(X),((X#cdsstorageFolder.cdscontainer)#cdscontainer.cdsobject)#cdsobject.title).
-define(CHILDCOUNT_FROM_CDSSTORAGEFOLDER(X), (X#cdsstorageFolder.cdscontainer)#cdscontainer.childCount).
-define(CHILDLIST_FROM_CDSSTORAGEFOLDER(X), (X#cdsstorageFolder.cdscontainer)#cdscontainer.childList).
%%
%% Funktionen
%%
%%-define(TREEWALKERFUN, fun(X) -> filelib:is_dir(filename:join(StartDir, X)) and (string:sub_string(filename:basename(X),1,1) /= ".") end).
-define(TREEWALKERFUN, fun(X) -> io:format("F ~p~n ", [X]),filelib:is_dir(X) and (string:sub_string(filename:basename(X),1,1) /= ".") end).
-define(FILELIST(X), filelib:fold_files(X, ".xml$", false, fun(F, L) -> [F|L] end, [])).
%%
%% XML
%%
-define(DIDL_LITE_HEADER, "<DIDL-Lite xmlns=\"urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:upnp=\"urn:schemas-upnp-org:metadata-1-0/upnp/\"/>\n").
-define(DIDL_LITE_TRAILER, "</DIDL-Lite>").
-define(SOAPENVELOPE,"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">\n").
%%
%% Konstanten
%%
-define(ALLSTATIONS, "Allstations").
-define(GENRES, "Genres").
-define(NO_SUCH_OBJECT,7 01).
-define(INVALID_CURRENT_TAG, 702).
-define(INVALID_NEW_TAG, 703).
-define(REQUIRED_TAG, 704).
-define(READ_ONLY_TAG, 705).
-define(PARAMETER_MISMATCH, 706).
-define(INVALID_SEARCH_CRITERIA, 708).
-define(INVALID_SORT_CRITERIA, 709).
-define(NO_SUCH_CONTAINER, 710).
-define(RESTRICTED_OBJECT, 711).
-define(BAD_METADATA, 712).
-define(RESTRICTED_PARENT_OBJECT, 713).
-define(NO_SUCH_SOURCE_RESOURCE, 714).
-define(SOURCE_RESOURCE_ACCESS_DENIED, 715).
-define(TRANSFER_BUSY, 716).
-define(NO_SUCH_FILE_TRANSFER, 717).
-define(NO_SUCH_DEST_RESOURCE, 718).
-define(DEST_RESOURCE_ACCESS_DENIED, 719).
-define(CANNOT_PROCESS_REQUEST, 720).