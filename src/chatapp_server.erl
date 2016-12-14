%%% Chat Server 

-module(chatapp_server).
-export([start/0, start_link/0, stop/0]).
-export([init/0]).

-author("Obaro Odiete").
-record(state, {clients, rooms, clientslist}).

-include("chatapp_config.hrl").

%% server external interface
start() -> 
	register(?SERVER, spawn(?MODULE, init, [])).

start_link() ->
	register(?SERVER, spawn_link(?MODULE, init, [])).

stop() -> 
	case erlang:whereis(?SERVER) of
		undefined -> true;
		_ -> ?SERVER ! shutdown
	end.


%% server internal interface
init() ->
	process_flag(trap_exit, true),
	io:format("Server started on ~s ~n", [?SERVER_NODE]),
	loop(#state{clients=orddict:new(), rooms=orddict:new(), clientslist=[]}). % clients is a dictionary with the client process ID as key and Username as value
																			  % rooms is a dictionary with room name as key amd room process ID as value

%% the server process itself
loop(S=#state{}) ->
	receive
		{Client, {login, Username}} -> % login user
			ClientList = S#state.clientslist,
			ClientExist = lists:member(Username, ClientList),

			if  ClientExist ->	
				Client ! {fail, username_already_exist},
				loop(S);
			not ClientExist ->
				erlang:link(Client),
				UpdatedClients = orddict:store(Client, Username, S#state.clients),
				UpdatedClientsList = lists:append(ClientList, [Username]),
				Client ! {ok, login_successful},
				io:format("~p connected ~n",[Username]),	% replace with logging
				loop(S#state{clients=UpdatedClients, clientslist=UpdatedClientsList})
			end;
		
		{Client, {create, Roomname}} -> % create room
			
			RoomExist = orddict:is_key(Roomname, S#state.rooms),
			if RoomExist ->
				Client ! {fail, room_already_exist},
				loop(S);
			not RoomExist ->	
				RoomPid = chatapp_chatroom:start_link(Roomname),
				UpdatedRooms = orddict:store(Roomname, RoomPid, S#state.rooms),
				Client ! {ok, chatroom_created},
				io:format("Created room: ~p ~n",[Roomname]),
				loop(S#state{rooms=UpdatedRooms})
			end;

		{Client, {list}} -> % list rooms
			Rooms = S#state.rooms,
			Roomslist = orddict:fetch_keys(Rooms),
			Client ! {list, Roomslist},
			loop(S);

		{Client, {join, Roomname}} -> % join room
			
			%% find out if room exist
			Rooms = S#state.rooms,
			RoomExist = orddict:is_key(Roomname, Rooms),
			
			if RoomExist ->					
				%% fetch the address of the room process
				RoomPid = orddict:fetch(Roomname, Rooms),
				Clientname = orddict:fetch(Client, S#state.clients),

				%% send message containing the client details to the chatroom address
				RoomPid ! {join, Client, Clientname},
				io:format("~p joined room ~p ~n", [Clientname, Roomname]);
			not RoomExist -> 
				Client ! {failed, room_does_not_exist}
			end,
			loop(S);
			
		shutdown -> % shutdown server
			exit(shutdown);
        
        {'EXIT', Pid, Why} -> % trapped exit messages from the chatrooms and connected clients

        	%% find out if it's from client or a chatroom
        	ClientExit = orddict:is_key(Pid, S#state.clients),

        	if ClientExit ->
				Clientname = orddict:fetch(Pid, S#state.clients),

				UpdatedClientsList = lists:delete(Clientname, S#state.clientslist),
				UpdatedClients = orddict:erase(Pid, S#state.clients),

				io:format("~p disconnected: ~p ~n",[Clientname, Why]),	% replace with logging
				loop(S#state{clients=UpdatedClients, clientslist=UpdatedClientsList});        		

        	not ClientExit ->
				[{FailedRoom, _}] = orddict:filter(fun(_key, Value) -> Value =:= Pid end, S#state.rooms),
				UpdatedRooms = orddict:erase(FailedRoom, S#state.rooms),

				io:format("~p is down: ~p ~n", [FailedRoom, Why]),
				loop(S#state{rooms=UpdatedRooms})
        	end;

        Unknown -> % unknown message
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(S)
	end.