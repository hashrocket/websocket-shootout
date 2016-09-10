#include <iostream>
#include <string>
#include <functional>

#include "server.h"

using namespace std;
using namespace uWS;
using namespace rapidjson;

server::server(int port)
	: port{port}
	, uwsES(uWS::MASTER)
	, uwsServer(this->uwsES, this->port, PERMESSAGE_DEFLATE, 0)
{
	uwsServer.onMessage(bind(&server::onMessage, this, placeholders::_1, placeholders::_2, placeholders::_3, placeholders::_4));
}

void server::echo(WebSocket socket, Value& payloadVal)
{
	Document doc;
	doc.SetObject();
	doc.AddMember("type", "echo", doc.GetAllocator());
	doc.AddMember("payload", payloadVal, doc.GetAllocator());

	StringBuffer buffer;
	Writer<StringBuffer> writer(buffer);
	doc.Accept(writer);

	socket.send(buffer.GetString(), buffer.GetSize(), OpCode::TEXT);
}

void server::broadcast(WebSocket socket, Value& payloadVal)
{
	Document broadcastDoc;
	broadcastDoc.SetObject();
	broadcastDoc.AddMember("type", "broadcast", broadcastDoc.GetAllocator());
	broadcastDoc.AddMember("payload", payloadVal, broadcastDoc.GetAllocator());

	StringBuffer broadcastBuffer;
	Writer<StringBuffer> broadcastWriter(broadcastBuffer);
	broadcastDoc.Accept(broadcastWriter);

	uwsServer.broadcast((char*) broadcastBuffer.GetString(), broadcastBuffer.GetSize(), OpCode::TEXT);

	Document resultDoc;
	resultDoc.SetObject();
	resultDoc.AddMember("type", "broadcastResult", resultDoc.GetAllocator());
	resultDoc.AddMember("payload", broadcastDoc["payload"], resultDoc.GetAllocator());

	StringBuffer resultBuffer;
	Writer<StringBuffer> resultWriter(resultBuffer);
	resultDoc.Accept(resultWriter);

	socket.send(resultBuffer.GetString(), resultBuffer.GetSize(), OpCode::TEXT);
}

void server::onMessage(uWS::WebSocket socket, char *message, size_t length, OpCode opCode) {
	Document doc;
	doc.Parse(message, length);
	if (doc.HasParseError()) {
		cout << "Unable to parse message";
		return;
	}

	Value& typeVal = doc["type"];
	Value& payloadVal = doc["payload"];

	auto type = string(typeVal.GetString());
	if (type == "echo") {
		echo(socket, payloadVal);
	}
	else if (type == "broadcast") {
		broadcast(socket, payloadVal);
	}
	else {
		cout << "bad type";
	}
}

void server::run()
{
	uwsES.run();
}
