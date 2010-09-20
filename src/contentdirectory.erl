%%% -------------------------------------------------------------------
%%% Author  : ulfangermann
%%% Description :
%%%
%%% Created : Jan 27, 2010
%%% -------------------------------------------------------------------
-module(contentdirectory).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/upnp.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([test/0]).
-export([get_nt/0, get_st/0, get_uri/0, get_service_type/0]).
-export([browse/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-record(state, {dirs}).

-define(UPNP_CONTENTDIRECTORY, "urn:schemas-upnp-org:service:ContentDirectory:1").
%% ====================================================================
%% External functions
%% ====================================================================
get_nt() ->
	?UPNP_CONTENTDIRECTORY.
get_st() ->
	get_nt().
get_uri() ->
	"/contentdirectory".
get_service_type() ->
	"".

browse(ObjectID, BrowseFlag, Filter, StartingIndex, RequestedCount, SortCriteria) ->	
	case BrowseFlag of 
		"BrowseMetaData" -> {ok, Result} = metadata_result(ObjectID),
							{ok, [1, 1, Result]}; 
		"BrowseDirectChildren" -> {ok, [Number_returned, Total_matches, Update_id]} = children_result(ObjectID);
		_ -> io:format("unknown Browserflag ~p~n", [BrowseFlag]),
			 {ok, []}
	end.	

metadata_result(ObjectID) ->
	{ok, []}.

children_result(ObjectID) ->
%%	filelib:fold_files(Root, ".*", true, fun (FileOrDirPath, Acc) -> [FileOrDirPath|Acc] end, []).
	ok.
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
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
    {ok, #state{dirs = get_dirs()}}.

get_dirs() ->
	{ok, Dirs} = application:get_env(?ERLMEDIASERVER_APP_FILE, dirs),
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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
test() ->
	{ok, Root} = file:get_cwd(),
	filelib:fold_files(Root, ".*", true, fun (FileOrDirPath, Acc) -> [FileOrDirPath|Acc] end, []).

