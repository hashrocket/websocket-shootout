#include <iostream>
#include <string>
#include <functional>
#include <thread>

#include "server.h"

using namespace std;
using namespace uWS;
using namespace rapidjson;

server::server(int port, int threadCount)
	: port{port}
	, uwsES(uWS::MASTER)
	, uwsServer(this->uwsES, this->port, PERMESSAGE_DEFLATE, 0)
{
	if (threadCount > 1) {
		threadServers.resize(threadCount);

		for (int i = 0; i < threadCount; i++) {
		  threadServerMutexes.push_back(make_unique<mutex>());

			new thread([this, i]{
					EventSystem tes(WORKER);
					this->threadServers[i] = new Server(tes, 0);

					this->threadServers[i]->onMessage(bind(&server::onMessage, this, placeholders::_1, placeholders::_2, placeholders::_3, placeholders::_4));

					tes.run();
			});
		}

		uwsServer.onUpgrade(bind(&server::onUpgrade, this, placeholders::_1, placeholders::_2, placeholders::_3, placeholders::_4, placeholders::_5));
	} else {
	  uwsServer.onMessage(bind(&server::onMessage, this, placeholders::_1, placeholders::_2, placeholders::_3, placeholders::_4));
  }
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

	if (threadServers.size() > 0) {
		for (int i = 0; i < threadServers.size(); i++) {
			std::lock_guard<std::mutex> lock(*threadServerMutexes[i]);
			threadServers[i]->broadcast((char*) broadcastBuffer.GetString(), broadcastBuffer.GetSize(), OpCode::TEXT);
		}
	} else {
		uwsServer.broadcast((char*) broadcastBuffer.GetString(), broadcastBuffer.GetSize(), OpCode::TEXT);
	}

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

void server::onUpgrade(uv_os_fd_t fd, const char *secKey, void *ssl, const char *extensions, size_t extensionsLength) {
	threadServers[rand() % threadServers.size()]->upgrade(fd, secKey, ssl, extensions, extensionsLength);
}

void server::run()
{
	uwsES.run();
}
