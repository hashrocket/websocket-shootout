#pragma once

#include <uWS.h>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>

class server {
public:
  server(int port);
  void run();

private:
  void echo(uWS::WebSocket socket, rapidjson::Value& payloadVal);
  void broadcast(uWS::WebSocket socket, rapidjson::Value& payloadVal);

  void onMessage(uWS::WebSocket socket, char *message, size_t length, uWS::OpCode opCode);

  int port;
  uWS::EventSystem uwsES;
  uWS::Server uwsServer;
};

