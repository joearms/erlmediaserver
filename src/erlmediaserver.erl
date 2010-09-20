%% Author: ua
%% Created: Jan 23, 2010
%% Description: TODO: Add description to ps3mediaserver
-module(erlmediaserver).

-behaviour(application).
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/upnp.hrl").
-include("/usr/local/lib/yaws/include/yaws.hrl").
-include("/usr/local/lib/yaws/include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([start/0, start/2, stop/0, stop/1, restart/0]).
-export([init/1, start_link/2]).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
RootDevice={root_device,
    	  {root_device, start_link, []},
           permanent,
		   10000,
           worker,
		   [root_device]},
Ssdp={ssdp,
    	  {ssdp, start_link, []},
           permanent,
		   10000,
           worker,
		   [ssdp]},
Mediaserver={mediaserver,
          {mediaserver, start_link, []},
           permanent,
           10000,
           worker,
          [mediaserver]},
RadioContentdirectory={radiocontentdirectory,
          {radiocontentdirectory, start_link, []},
           permanent,
           10000,
           worker,
          [radiocontentdirectory]},
RadioConnectionmanager={radioconnectionmanager,
          {radioconnectionmanager, start_link, []},
           permanent,
           10000,
           worker,
          [radioconnectionmanager]},
Contentdirectory={contentdirectory,
          {contentdirectory, start_link, []},
           permanent,
           10000,
           worker,
          [contentdirectory]},
Connectionmanager={connectionmanager,
          {connectionmanager, start_link, []},
           permanent,
           10000,
           worker,
          [connectionmanager]},
{ok, {{one_for_one, 3, 10},
		   [RootDevice,
			Ssdp,
			RadioContentdirectory,
			RadioConnectionmanager,
			Connectionmanager,
			Contentdirectory,
			Mediaserver
			]}}.
%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/0
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start() ->
	application:start(?MODULE).

%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _Args) ->
	Args = lists:map(fun (Var) -> {ok, Value} = application:get_env(?ERLMEDIASERVER_APP_FILE , Var),Value end,[port, working_dir]),
	case application:start(yaws) of 
		ok -> set_conf(Args),
			  error_logger:info_msg("yaws startet ~n");		
		Error -> 
			{stop, Error}
	end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(_Type, _Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% --------------------------------------------------------------------
%% Func: restart/0
%% Returns: any
%% --------------------------------------------------------------------
restart() ->
    stop(),
    start().

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
	application:stop(?MODULE).

stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
set_conf([Port, WorkingDir]) ->
	{ok, WorkDir} = file:get_cwd(),
	GC = #gconf{
				trace = {true, traffic},
				logdir = WorkDir ++ "/yaws_logs",
				yaws = "eMedia Server 1.0"
				},
	SC = #sconf{
				port = Port, 
				servername = os_info:get_ip_as_string(), listen = {0,0,0,0},
				docroot = WorkDir ++ "/docroot", 
				appmods = [{"/description/fetch", description_handler},
						   {"/images/erlmedia.jpg", description_handler},						   						   
				   		   {"/service/radioconnectionmanager", radioconnectionmanager_handler},
						   {"/service/radioconnectionmanager/control", radioconnectionmanager_handler},					
				   		   {"/service/radiocontentdirectory/control", radiocontentdirectory_handler},
						   {"/service/radiocontentdirectory", radiocontentdirectory_handler},
						   {"/service/contentdirectory", contentdirectory_handler},
						   {"/service/contentdirectory/control", contentdirectory_handler},
						   {"/service/connectionmanager", connectionmanager_handler},
						   {"/service/connectionmanager/control", connectionmanager_handler}]
				},
	case catch yaws_api:setconf(GC, [[SC]]) of
		ok -> {ok, started};
		Error -> {stop, Error}
 	end.