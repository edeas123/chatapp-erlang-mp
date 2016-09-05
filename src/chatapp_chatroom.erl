%% Chat room
-module(chatapp_chatroom).
-export([start/1, start_link/1]).
-export([init/1]).

-author("Obaro Odiete").

-record(state, {name, clients, messages}).
-include("chatapp_config.hrl").

%% chatroom interface
start(Name) -> 
	spawn(?MODULE, init, [Name]).
	
start_link(Name) ->
	spawn_link(?MODULE, init, [Name]).

init(Name) ->
	loop(#state{name=Name, clients=orddict:new(), messages=[]}).


%% the chatroom process itself
loop(S = #state{}) ->
	receive
		{join, Client, ClientName} ->
			UpdatedClients = orddict:store(Client, ClientName, S#state.clients),
			Roomname = S#state.name,
			
			% retrieve old messages to send to new client
			Messages = S#state.messages,

			% send message to the client containing the chatroom address
			Client ! {joined, Roomname, self(), Messages},
			loop(S#state{clients=UpdatedClients});

		{leave, Client} ->
			UpdatedClients = orddict:erase(Client, S#state.clients),
			loop(S#state{clients=UpdatedClients});
		
		{message, Client, Message} ->

			%% verify that client is member of chatroom
			RoomMember = orddict:is_key(Client, S#state.clients),
			if RoomMember ->
				
				%% get sender's name
				ClientName = orddict:fetch(Client, S#state.clients),
				Localtime = calendar:local_time(),

				%% save message in state so as to push to new users
				UpdatedMessages = lists:append(S#state.messages, [{Localtime, ClientName, Message}]),
				
				Clients = S#state.clients,
				ClientPids = orddict:fetch_keys(Clients),

				%% send message to all joined users
				[Pid ! {room, {Localtime, ClientName, Message}} || Pid <- ClientPids],

				%% loop
				loop(S#state{messages=UpdatedMessages});
			not RoomMember ->
				%% do nothing
				loop(S)
			end;			

        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(S)
    after ?CHATROOM_LIFETIME ->
    	exit(expired)
	end.
