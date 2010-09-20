%%% -------------------------------------------------------------------
%%% Author  : ulfangermann
%%% Description :
%%%
%%% Created : Apr 7, 2010
%%% -------------------------------------------------------------------
-module(radiocontentdirectory).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/upnp.hrl").
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% External exports

-compile(export_all).

-export([get_nt/0, get_st/0, get_uri/0, get_service_type/0]).
-export([initMediaTree/1, loadCDSFolder/1]).
-export([search/1, browse/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-record(state, {dirs}).
-record(acc, {storages=[] , subDirs=[]}).
-define(UPNP_CONTENTDIRECTORY, "urn:schemas-upnp-org:service:ContentDirectory:1").
%% ====================================================================
%% External functions
%% ====================================================================
get_nt() ->
	?UPNP_CONTENTDIRECTORY.
get_st() ->
	get_nt().
get_uri() ->
	"/radio_content_directory".
get_service_type() ->
	?UPNP_CONTENTDIRECTORY.

search(SearchArguments) when is_record(SearchArguments, searchArguments) ->
	gen_server:call(?MODULE, {search, SearchArguments}).

browse(BrowseArguments) when is_record(BrowseArguments, browseArguments) ->
	gen_server:call(?MODULE, {browse, BrowseArguments}).
	
%% ====================================================================
%% Server functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    State = #state{dirs = get_dirs()}, 
	%%initMediaTree(State),
	{ok, State}.

%% --------------------------------------------------------------------
%%
%% --------------------------------------------------------------------

get_dirs() ->
	{ok, [Dirs]} = application:get_env(?ERLMEDIASERVER_APP_FILE, dirs),
	Dirs.	

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({search, SearchArguments}, _From, State) ->
    SearchResult = #searchResult{},
    {reply, SearchResult, State};

handle_call({browse, BrowseArguments}, _From, State) ->
	error_logger:info_msg("browse with args : ~p~n", [BrowseArguments]),
    BrowseResult = #browseResult{},
    {reply, BrowseResult, State}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
initMediaTree(State) ->
	loadCDSFolder(State#state.dirs),	
	ok.

loadCDSFolder(RootDir) ->
	%%file:set_cwd(RootDir),
	Acc = walkTheTree([RootDir], "*", #acc{storages=[]}),
	io:format("---- ACC ---- ~p~n", [Acc#acc.storages]).
	

%% Ermittle zu einen Dir alle SubDirs
walkTheTree([Dir| _], Filter, Acc) ->
	io:format("\t 1. ------~p~n", [Dir]),
	file:set_cwd(Dir),
	case lists:filter(?TREEWALKERFUN, filelib:wildcard(Filter, ".")) of
		[] -> walkTheTree([], Filter, Acc);
		Dirs -> walkTheTree(Dirs,  Filter, Acc)
	end;

%% Ermittle für eine Liste von Dirs die SubDirs 
walkTheTree([Dir|RestDirs], Filter, Acc) ->
	io:format("\t\t 2. ------~p~n", [Dir]),
	walkTheTree(Dir, Filter, Acc) ,
	walkTheTree(RestDirs, Filter, Acc);

%% Verabeite das aktuelle Dir
%% Es gibt keine subdirs
walkTheTree([],  _Filter, #acc{subDirs=[]}=Acc) ->
	io:format("\t\t\t 3. ------~n"),	
	Files = ?FILELIST("."),
	{ok, AbsDirName} = file:get_cwd(),
	DirName = filename:basename(AbsDirName),
	io:format("\t\t\t 3.-------- FileInfo ~p~n", [DirName]),
	Storage=?CDSSTORAGEFOLDER("ParentId", DirName, "TTTTT", createCDS(Files)),
	%%io:format("Files ~p~n", [Storage]),
	file:set_cwd(".."),
	Acc#acc{storages=[Storage | Acc#acc.storages]}.

%% Erzeuge aus der Liste der XML Files die Liste der CDSObjekte 
createCDS(Files) ->
	[xml_to_cds:createCDSObject(X) || X <- Files ].

