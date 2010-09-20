-module(sax).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {stack = [],  posStack=[], nsStack=[]}).
-record(attribute, {localName, prefix = [], uri = [], value}).

%%xmerl_sax_parser:file("simple.xml", [{event_fun, fun sax:parser/3}, {event_state, sax:new_state()}]).

new_state() ->
    #state{stack = [], posStack=[]}.

parser({startElement, Uri, LocalName, QualifiedName, Attributes} = Elem, _, State = #state{stack = [], posStack = []}) ->
	%%io:format("Starting1 Uri: ~p, LocalName : ~p, QualifiedName : ~p, Attributes : ~p~p~n", [Uri, LocalName, QualifiedName, Attributes, State#state.nsStack]),
	Name = makeName(QualifiedName),
	Attributes1 = lists:append(Attributes, [makeAttribute(X, Y) || {X, Y} <- State#state.nsStack]), 
	State#state{stack = [#xmlElement{name = Name,
									expanded_name = makeExpandedName(Uri, LocalName),
                                    pos = 1,
                                    nsinfo = makeNsInfo(QualifiedName),
                                    namespace = makeNs(State#state.nsStack, #xmlNamespace{}),
                                    parents = [],
                                    attributes = processAttributes(Attributes1, State),
                                    content = []}],
               			posStack = [0]
			   };

		
parser({startElement, Uri, LocalName, QualifiedName, Attributes},_ , State = #state{stack = [Parent | _], posStack = [Pos | _]}) ->
	%%io:format("Starting2 Uri: ~p, LocalName : ~p, QualifiedName : ~p, Attributes : ~p NSStack : ~p~n", [Uri, LocalName, QualifiedName, Attributes, State#state.nsStack]),
	Name = makeName(QualifiedName),
	Attributes1 = lists:append(Attributes, [makeAttribute(X, Y) || {X, Y} <- State#state.nsStack]),
	State#state{stack = [#xmlElement{name = Name,
									expanded_name = makeExpandedName(Uri, LocalName),
                                    pos = Pos + 1,
                                    nsinfo = makeNsInfo(QualifiedName),
                                    namespace = makeNs(State#state.nsStack, #xmlNamespace{}),
                                    parents = getParentsFromStack(State#state.stack, []),
                                    attributes = processAttributes(Attributes1, State),
                                    content = []} | State#state.stack],
               			posStack = [0 | State#state.posStack]
			   };
	

parser({characters, Characters}, _, State=#state{stack=[#xmlElement{content=[#xmlText{value=Text}=FirstPart|Rest]} = Element|Tail]}) ->	
	State#state{stack=[Element#xmlElement{content=[FirstPart#xmlText{value =Text++Characters}|Rest]}|Tail]};

parser({characters, Characters}, _, State=#state{stack=[#xmlElement{content=Content}=Element|Tail], posStack=[NrOfElements|PosTail]}) ->
	State#state{stack=[Element#xmlElement{content = [#xmlText{value = changeCharacters(Element#xmlElement.name, Characters), 
															  parents = getParentsFromStack(State#state.stack, []),
                                                              pos = NrOfElements + 1} | Content]} | Tail],
                                           					  posStack = [NrOfElements + 1 | PosTail]};
	
parser({endElement, _Uri, _LocalName, _Prefix}, _, State = #state{stack = [#xmlElement{content = Content} = Top]}) ->
    State#state{stack = [Top#xmlElement{content = lists:reverse(Content)}]};

parser({endElement, _Uri, _LocalName, _Prefix}, _, #state{stack = [#xmlElement{content = ChildContent} = Child | [#xmlElement{content = ParentContent} = Parent | Tail]],
          												  posStack = [_NrOfChildEls | [NrOfElements | PosTail]]} = State) -> 
	State#state{stack = [Parent#xmlElement{content = [Child#xmlElement{content = lists:reverse(ChildContent)} | ParentContent]} |Tail],
    									   posStack = [NrOfElements + 1 | PosTail]};

parser(endDocument, _, State) ->
	State#state.stack;

parser({startPrefixMapping, Prefix, Uri}, _, State) ->
	%%io:format("startPrefixMapping ~p~p ~p~n", [Prefix, Uri, State#state.nsStack]),
	
	case [{X, Y} || {X, Y} <- State#state.nsStack, X == Prefix] of
		[] -> State#state{nsStack = [{Prefix, Uri} | State#state.nsStack]};
		Other -> State
	end;

parser({endPrefixMapping, Prefix}, _, State) -> 
    %%io:format("endPrefixMapping ~p~n", [Prefix]),
    State#state{nsStack=[{X,Y} || {X,Y} <- State#state.nsStack, X /= Prefix]};


parser(_Other, _, State) -> 
    %%io:format("------- Other ~p~p~n", [ Other, State]),
    State.

changeCharacters('URLBase', _Characters) ->
	"http://" ++ root_device:get_ip_port();
	
changeCharacters('UDN', _Characters) ->
	"uuid:" ++ root_device:get_uuid();

changeCharacters('presentationURL', Characters) ->
	"http://" ++ root_device:get_ip_port() ++ Characters;
	
changeCharacters(_, Characters ) ->
	Characters.

getParentsFromStack([], Acc) ->
 	Acc;
getParentsFromStack([#xmlElement{name = Name, pos = Pos} | Tail], Acc) ->
	getParentsFromStack(Tail, [{Name, Pos} | Acc]).

makeAttribute([], Uri) ->
	{Uri, [], 'xmlns', Uri};
makeAttribute(Prefix, Uri) ->
	{Uri, Prefix, list_to_atom("xmlns:" ++ Prefix), Uri}.

processAttributes(Attributes, State) ->
	%%io:format("1--------------~p~n", [Attributes]),
	processAttributes(Attributes, State, 1, []).
processAttributes([], _State, _Count, Acc) ->
	%%io:format("2--------------~p~n", [Acc]),
	lists:reverse(Acc);
processAttributes([{Uri, Prefix, AttributeName, Value} | Tail], State, Count, Acc) ->
	%%io:format("3--------------~n"),
	processAttributes(Tail, State, Count + 1, [#xmlAttribute{name =  AttributeName,
															expanded_name = makeExpandedName(Uri, AttributeName),
          													nsinfo = makeNsInfo({Prefix, AttributeName}),
															pos = Count,
															value = Value} | Acc]).


makeName({[], Local}) ->
	%%io:format("start makeName 1 : ~p~n", [Local]),
	list_to_atom_or_not(Local);
makeName({Prefix, Local}) ->
	%%io:format("start makeName 2 : ~p ~p~n", [Prefix, Prefix]),
	list_to_atom_or_not(Prefix ++ ":" ++ Local).

makeNsInfo({[], _}) -> 
	%%io:format("1 makeNSInfo~n"),
	[];
makeNsInfo({Prefix, Local}) -> 
	%%io:format("2 makeNSInfo ~p~p~n", [Prefix, Local]),
	{Prefix, Local}.

%% #xmlNamespace{default = 'urn:schemas-upnp-org:device-1-0', nodes = [{"dlna",'urn:schemas-dlna-org:device-1-0'}]},
makeNs([], Ns) ->
	%%error_logger:info_msg("1. makeNS ~p~p~n", [Ns]),
	Ns;

makeNs(NSList, #xmlNamespace{nodes = Nodes} = ParentNs) ->
	%%error_logger:info_msg("2. makeNS ~p~n", [NSList]),
	[Default] = [list_to_atom(Y) || {X, Y} <- NSList, erlang:length(X) == 0],
	%%error_logger:info_msg("2a. makeNS ~p~n", [NSList]),
	ParentNs#xmlNamespace{default=Default, nodes = Nodes ++ [{X, list_to_atom(Y)} || {X, Y} <- NSList, erlang:length(X) > 0]}.


% string() | {URI,Local} | {"xmlns",Local}
makeExpandedName([], Local) ->
	%%io:format("makeExpandedName 1 ~p~n", [Local]),
	list_to_atom_or_not(Local);
makeExpandedName(Uri, Local) ->
	%%io:format("makeExpandedName 2 ~p~p~n", [Uri,Local]),
	{list_to_atom_or_not(Uri), list_to_atom_or_not(Local)}.

list_to_atom_or_not(String) ->
  try list_to_atom(String)
  catch
    _:_ -> String
  end.

makeNS_test() ->
	TestNamespace = [{[], "urn:schemas-upnp-org:device-1-0"},{"dlna","urn:schemas-dlna-org:device-1-0"}],
	error_logger:info_msg("Result ~p~n", [ makeNs(TestNamespace, #xmlNamespace{})]),
	TestNamespace1 = [{[], "urn:schemas-upnp-org:device-1-0"}],
	error_logger:info_msg("Result ~p~n", [ makeNs(TestNamespace1, #xmlNamespace{})]).

makeAttribute_test() ->
	makeAttribute([], "Test"),
	makeAttribute("Prefix", "Test").

%%	TestNamespace = [{[], "urn:schemas-upnp-org:device-1-0"},{"dlna","urn:schemas-dlna-org:device-1-0"}],
%%	[makeAttribute(X, Y) || {X, Y} <- TestNamespace].
	
	

