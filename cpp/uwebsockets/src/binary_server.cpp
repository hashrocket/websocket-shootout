#include <iostream>
#include <string>
#include <functional>
#include <thread>

#include "binary_server.h"

using namespace std;
using namespace uWS;

binary_server::binary_server(int port) :
	port{port}
{
  hub.onMessage(bind(&binary_server::onMessage, this, placeholders::_1, placeholders::_2, placeholders::_3, placeholders::_4));
}

void binary_server::echo(uWS::WebSocket<uWS::SERVER> socket, char *msg, size_t length)
{
	socket.send(msg, length, OpCode::BINARY);
}

void binary_server::broadcast(uWS::WebSocket<uWS::SERVER> socket, char *msg, size_t length)
{
	hub.getDefaultGroup<uWS::SERVER>().broadcast(msg, length, OpCode::BINARY);

	// Mutating in place should be safe based on looking at uWebSockets source code, but it's not explicitly documented.
	msg[0] = 'r';

	socket.send(msg, length, OpCode::BINARY);
}

void binary_server::onMessage(uWS::WebSocket<uWS::SERVER> socket, char *message, size_t length, OpCode opCode) {
	auto type = message[0];
	if (type == 'e') {
		echo(socket, message, length);
	}
	else if (type == 'b') {
		broadcast(socket, message, length);
	}
	else {
		cout << "bad type";
	}
}

bool binary_server::run()
{
	if (hub.listen(port)) {
		hub.run();
	}
	return false;
}
