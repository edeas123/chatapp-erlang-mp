
%%% User interface for the clients

%%% login(Username)
%%%		Allow a user with Username to connect to the server with a suitable name.
%%%		If the name is already logged-in, refuse connection with a suitable
%%%		error message that user with that name already logged-in.
%%% logouy(Username)
%%%		Disconnect Username from the server.
%%%	create_room(Roomname)
%%%		Create a chatroom with name, Roomname. 
%%%		The roomname must be unique. If name already exists for chatroom, 
%%%		respond with message that chatroom with that name already exists.
%%% list_rooms()
%%%		List the existing rooms
%%% join_room(Roomname)
%%%		Request to join chatroom with name, Roomname.
%%%		If chatroom with that name does not exist, respond with message that
%%%		that chatroom does not exist.
%%% leave_room(Roomname)
%%%		Request to leave chatroom with name, Roomname.
%%%		If user is not in chatroom. Do nothing.
%%% message_room(Roomname, Message)
%%%		Send Message to Roomname.

-module(chatapp_user_interface).
-export([login/1, logout/0, create_room/1, list_rooms/0, join_room/1, leave_room/1, message_room/2]).

-include("chatapp_config.hrl").

login(Username) -> 
	LoggedIn = check_login(),
	if LoggedIn -> 
			client_already_logged_on;
		true -> 
			register(?CLIENT, spawn(?CLIENT, init, [Username]))
	end.

logout() ->
	%% TODO: Check later
	?CLIENT ! {logout},
	ok.

create_room(Roomname) ->
	LoggedIn = check_login(),
	if LoggedIn -> 
		?CLIENT ! {create, Roomname},
		ok;
	not LoggedIn -> 
		client_not_logged_on
	end,
	ok.

list_rooms() ->
	LoggedIn = check_login(),
	if LoggedIn -> 
		?CLIENT ! {list},
		ok;
	not LoggedIn -> 
		client_not_logged_on
	end.

join_room(Roomname) ->
	LoggedIn = check_login(),
	if LoggedIn -> 
		?CLIENT ! {join, Roomname},
		ok;
	not LoggedIn -> 
		client_not_logged_on
	end.

leave_room(Roomname) ->
	LoggedIn = check_login(),
	if LoggedIn -> 
		?CLIENT ! {leave, Roomname},
		ok;
	not LoggedIn -> 
		client_not_logged_on
	end.

message_room(Roomname, Message) ->
	LoggedIn = check_login(),
	if LoggedIn -> 	
		?CLIENT ! {message, Roomname, Message},
		ok;
	not LoggedIn -> 
		client_not_logged_on
	end.

check_login() ->
	case whereis(?CLIENT) of
		undefined -> false;
		_ -> true
	end.
