-define(SERVER, chatapp_server). % server process
-define(CLIENT, chatapp_client). % client process
-define(SERVER_NODE, 'server@DESKTOP-N6OP4IN'). % TODO: remove
-define(TIMEOUT, 8000). % global timeout
-define(CHATROOM_LIFETIME, 7 * 24 * 60 * 60 * 1000). % chatroom lifetime