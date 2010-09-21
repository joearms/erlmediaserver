%% Author: ua
%% Created: Jan 17, 2010
%% Description: TODO: Add description to os_info
-module(os_info).

%%
%% Include files
%%
-include("../include/upnp.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-compile(export_all).
-export([test/0]).
-export([get_type/0, get_version/0, get_os_description/0, get_ip/0, get_ip/1, get_ip_as_string/0, get_ip_as_string/1]).
%%
%% API Functions
%%
get_all() ->
	get_all(get_os()).

get_type() ->
	get_type(get_os()).

get_version() ->
	get_version(get_os()).

get_os_description() ->
	get_os_description(get_os()).

get_os() ->
	{Osfamily, Osname} = os:type(),
	case Osname of
		darwin -> case check_if_sw_vers_avaible() of
					  true -> mac;
					  false -> Osfamily
				  end;
		_ -> Osfamily
	end.
check_if_sw_vers_avaible() ->
	filelib:is_file('/usr/bin/sw_vers').

get_all(mac) ->
	os:cmd('sw_vers');

get_all(unix) ->
	os:cmd('uname -a').

get_type(mac) -> 
	Type = os:cmd('sw_vers -productName'),
	string:strip(Type, right, 10).
	
get_version(mac) ->
	Version = os:cmd('sw_vers -productVersion'),
	string:strip(Version, right, 10);

get_version(unix) ->
	os:cmd('uname -r').
	
get_os_description(mac) ->
	List = [get_type(mac),
			" ",
			get_version(mac),
			" ",
			get_upnp()		
			],
	lists:append(List);

get_os_description(unix) ->
	List = [os:cmd('uname -v'),
  			" ",
			get_upnp()
			],
	lists:append(List).

get_upnp() ->
	{ok, Upnp} = application:get_env(?ERLMEDIASERVER_APP_FILE, upnp),
	Upnp.

get_ip() ->
	{ok, List} = inet:getiflist(),
	A = [X || X <- List, string:str(X, "vmnet") == 0,  string:str(X, "lo0") == 0 ],
	I = [D || Y <- A, (D = get_ip(Y)) /= []],
	case erlang:length(I) of
		1 -> lists:nth(1, I);
		_ -> get_loopback()
	end.

get_ip(If) ->
	case inet:ifget(If, [addr]) of
		{ok, []} -> [];
		{_, [{_, Ip}]} -> Ip
	end.

get_ip_as_string() ->
	case get_ip() of
		error -> get_loopback();
		Ip -> inet_parse:ntoa(Ip)
	end.

get_ip_as_string(Ip) ->
	inet_parse:ntoa(Ip).

get_loopback() ->
	get_ip("lo0").

%%
%% Local Functions
%%
get_ip_as_string_test() ->
	?assertEqual("192.168.2.32", get_ip_as_string({192,168,2,32})).

test() ->
	{ok, List} = inet:getiflist(),
	A = [X || X <- List, string:str(X, "vmnet") == 0,  string:str(X, "lo0") == 0 ],
	[Y || Y <- A, get_ip(Y) /= []].

get_os_test() ->
	?assertEqual(mac, get_os()).

get_os_description_test() ->
	application:load(erlmediaserver),
	?assertEqual("Mac OS X 10.6.4 UPnP/1.0 ERL_PMS/1.0", get_os_description()).
	