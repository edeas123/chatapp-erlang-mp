%% Comments
-module(chatapp_client).
-export([init/1]).

-author("Obaro Odiete").
-include("chatapp_config.hrl").

%% client process
init(Username) ->
	{?SERVER, ?SERVER_NODE} ! {self(), {login, Username}},
	receive
		{fail, Why} ->
			io:format("~p ~n", [Why]), %Timestamp and log in a client logfile
			exit(normal);
		{ok, What} ->
			io:format("~p ~n", [What])
	after ?TIMEOUT ->
		io:format("No response from server~n", []),
		exit(timeout)
	end,
	loop(orddict:new()). % the state is a dictionary with keys, the roomname and value, the chatroom PID

loop(S) ->
	receive
		{create, Roomname} ->
			{?SERVER, ?SERVER_NODE} ! {self(), {create, Roomname}},
			receive
				{fail, Why} ->
					io:format("~p ~n", [Why]); 
				{ok, What} ->
					io:format("~p ~n", [What])
			after ?TIMEOUT ->
				io:format("No response from server~n", [])
			end,			
			loop(S);

		{list} ->
			{?SERVER, ?SERVER_NODE} ! {self(), {list}},
			receive
				{list, RoomList} ->
					io:format("Available chatrooms: ~p ~n",  [RoomList]),
					loop(S)

			after ?TIMEOUT ->
				io:format("No response from server~n", []),
				loop(S)
			end;
		
		{join, Roomname} ->
			%% Check if a member of room
			RoomMember = orddict:is_key(Roomname, S),
			if RoomMember ->
				io:format("user_already_in_chatroom. ~n", []),
				loop(S);
			not RoomMember ->
				{?SERVER, ?SERVER_NODE} ! {self(), {join, Roomname}},
				receive
					{joined, Roomname, RoomPid, Messages} -> 
						io:format("Joined room ~p ~n", [Roomname]),

						% output message archives from chatroom
						[io:format("(~p-~p-~p ~p:~p) ~p: ~p ~n", 
							[D, MM, Y, H, M, Sender, Message]) || {{{Y,MM,D},{H,M,_}}, Sender, Message} <- Messages],
						
						% update the state of the client with the new connected chatroom details
						UpdatedState = orddict:store(Roomname, RoomPid, S),

						% monitor the chatroom
						% io:format("Chatroom ~p ~n", [RoomPid]),
						erlang:monitor(process, RoomPid),

						% loop
						loop(UpdatedState);
					{failed, Why} ->
						io:format("~p ~n", [Why]),
						loop(S)
				after ?TIMEOUT ->
					io:format("Join room: No response from server or chatroom ~n", [])
				end					
			end;

		{leave, Roomname} ->

			%% Check if a member of room
			RoomMember = orddict:is_key(Roomname, S),
			if RoomMember ->
				%% Get the address of the chatroom
				RoomPid = orddict:fetch(Roomname, S),
				
				%% Send message directly to the chatroom to leave
				RoomPid ! {leave, self()},

				%% Remove address of chatroom from state
				UpdatedState = orddict:erase(Roomname, S),
				io:format("left_room. ~n", []),
				loop(UpdatedState);
			
			not RoomMember ->
				io:format("user_not_in_chatroom. ~n", []),
				loop(S)
			end;

		{message, Roomname, Message} ->
			%% Check if a member of room
			RoomMember = orddict:is_key(Roomname, S),
			if RoomMember ->
				%% Get the address of the chatroom
				RoomPid = orddict:fetch(Roomname, S),
				
				%% Send message directly to the chatroom
				RoomPid ! {message, self(), Message};

			not RoomMember ->
				io:format("user_not_in_chatroom. ~n", [])
			end,
			loop(S);

		{room, {{{Y,MM,D},{H,M,_}}, Sender, Message}} ->

			io:format("(~p-~p-~p ~p:~p) ~p: ~p ~n", [D, MM, Y, H, M, Sender, Message]),
			loop(S);

		{logout} ->
			exit(normal);

		{'DOWN', _ref, process, Pid, Why} ->
			%% handle cases when the chatroom goes down on its own
			%% Since the client monitors the chatroom
			
			[{FailedRoom, _}] = orddict:filter(fun(_key, Value) -> Value =:= Pid end, S),
			UpdatedState = orddict:erase(FailedRoom, S),
			io:format("~p is down: ~p ~n", [FailedRoom, Why]),
			loop(UpdatedState);

        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
			loop(S)
	end.