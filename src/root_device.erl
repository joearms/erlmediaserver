%% Author: ua
%% Created: Apr 20, 2010
%% Description: TODO: Add description to root_device
-module(root_device).


-behaviour(gen_server).
%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("../include/upnp.hrl").
%%
%% Exported Functions
%%
-export([get_ip/0, get_port/0, get_services/0, get_description_uri/0, get_uuid/0, get_os/0, createRootdevice/0]).
-export([get_ip_as_string/0, get_ip_port/0, get_os_info/0, get_root_device/0, get_service/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0]).
-export([get_nt/0, get_st/0, get_uri/0, get_service_type/0]).

-record(state, {rootdevice}).

-define(UPNP_ROOTDEVICE, "upnp:rootdevice").
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{rootdevice=createRootdevice()}}.
%%
%% API Functions
%%
get_root_device() ->
	gen_server:call(?MODULE, {get_root_device}). 

get_ip() ->
	gen_server:call(?MODULE, {get_ip}). 
	
get_ip_as_string() ->
	gen_server:call(?MODULE, {get_ip_as_string}).

get_port() ->
	gen_server:call(?MODULE, {get_port}).

get_services() ->
	gen_server:call(?MODULE, {get_services}).

get_service(Search_Type) ->
	gen_server:call(?MODULE, {get_service, Search_Type}).
	
get_description_uri() ->
	gen_server:call(?MODULE, {get_description_uri}).

get_uuid() ->
	gen_server:call(?MODULE, {get_uuid}).

get_os() ->
	gen_server:call(?MODULE, {get_os}).

get_os_info() ->
	gen_server:call(?MODULE, {get_os_info}).

get_ip_port() ->
	gen_server:call(?MODULE, {get_ip_port}).
get_nt() ->
	?UPNP_ROOTDEVICE.
get_st() ->
	get_nt().
get_service_type() ->
	"upnp:rootdevice".
get_uri() ->
	"/description/fetch".

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
handle_call({get_root_device}, _From, State) ->
	{reply, State#state.rootdevice, State};
handle_call({get_ip}, _From, State) ->
	{reply, get_ip(State#state.rootdevice), State};
handle_call({get_ip_as_string}, _From, State) ->
	{reply, get_ip_as_string(State#state.rootdevice), State};
handle_call({get_port}, _From, State) ->
	{reply, get_port(State#state.rootdevice), State};
handle_call({get_services}, _From, State) ->
	{reply, get_services(State#state.rootdevice), State};
handle_call({get_description_uri}, _From, State) ->
	{reply, get_description_uri(State#state.rootdevice), State};
handle_call({get_os}, _From, State) ->
	{reply, get_os(State#state.rootdevice), State};
handle_call({get_os_info}, _From, State) ->
	{reply, get_os_info(State#state.rootdevice), State};
handle_call({get_uuid}, _From, State) ->
	{reply, get_uuid(State#state.rootdevice), State};
handle_call({get_ip_port}, _From, State) ->
	{reply, get_ip_port(State#state.rootdevice), State};
handle_call({get_service, Search_Type}, _From, State) ->
	{reply, get_service(get_services(State#state.rootdevice), Search_Type), State}.

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
handle_info(Info, State) ->
	error_logger:info_report("Info : ~n~p : ", [Info]),
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

%%
%% Local Functions
%%	
get_ip(RootDevice) when is_record(RootDevice, rootdevice)->
	RootDevice#rootdevice.ip.

get_ip_as_string(RootDevice) when is_record(RootDevice, rootdevice)->
	os_info:get_ip_as_string(RootDevice#rootdevice.ip).

get_port(RootDevice) when is_record(RootDevice, rootdevice)->
	RootDevice#rootdevice.port.

get_services(RootDevice) when is_record(RootDevice, rootdevice) ->
	RootDevice#rootdevice.services.

get_description_uri(RootDevice) when is_record(RootDevice, rootdevice) ->
	RootDevice#rootdevice.descriptionuri.

get_uuid(RootDevice) when is_record(RootDevice, rootdevice) ->
	RootDevice#rootdevice.uuid.

get_os(RootDevice) when is_record(RootDevice, rootdevice) ->
	RootDevice#rootdevice.os.

get_os_info(RootDevice) when is_record(RootDevice, rootdevice) ->
	RootDevice#rootdevice.os.

get_ip_port(RootDevice) when is_record(RootDevice, rootdevice) ->
	os_info:get_ip_as_string(RootDevice#rootdevice.ip) ++ ":" ++ integer_to_list(RootDevice#rootdevice.port).

get_service(Services, ST) ->
	case [X || X <- Services, string:equal(ST, X:get_st())] of
		[] -> {error, "no service found"};
		[Service] -> {ok, Service}
	end.

createRootdevice() ->
	{ok, P} = application:get_env(?ERLMEDIASERVER_APP_FILE, port),
	{ok, S} = application:get_env(?ERLMEDIASERVER_APP_FILE, services),
	#rootdevice{uuid=uuid:v4_as_string(), os=os_info:get_os_description(),
				ip=os_info:get_ip(), port=P,services=S}.	
%%
%% Test Functions
%% 
get_ip_port_test() ->
	R=createRootdevice(),
	?assertEqual("192.168.2.32:5001", get_ip_port(R)).	

get_ip_test() ->
	R=createRootdevice(),
	?assertEqual({192,168,2,32}, get_ip(R)).

get_port_test() ->
	R=createRootdevice(),
	?assertEqual(5001, get_port(R)).

get_services_test() -> 
	R=createRootdevice(),
	?assertEqual(5, erlang:length(get_services(R))).

get_description_uri_test() ->
	R=createRootdevice(),
	?assertEqual("/description/fetch", get_description_uri(R)).	
	
get_uuid_test() ->
	_R=createRootdevice().
	%%?assertEqual(123, get_uuid(R)).	

get_os_test() ->
	R=createRootdevice(),
	?assertEqual("Mac OS X 10.6.2 UPnP/1.0 ERL_PMS/1.0", get_os(R)).

get_service_test() ->
	?assertEqual({ok,radiocontentdirectory}, get_service([root_device, mediaserver, contentdirectory, connectionmanager, radiocontentdirectory], "urn:schemas-upnp-org:service:RadioContentDirectory:1")),
	?assertEqual({error,"no service found"}, get_service([root_device, mediaserver, contentdirectory, connectionmanager, radiocontentdirectory], "wrong description")).
	
	
