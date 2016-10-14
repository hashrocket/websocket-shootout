#pragma once

#include <uWS.h>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>

class server {
public:
  server(int port);
  bool run();

private:
  void echo(uWS::WebSocket<uWS::SERVER> socket, rapidjson::Value& payloadVal);
  void broadcast(uWS::WebSocket<uWS::SERVER> socket, rapidjson::Value& payloadVal);

  void onMessage(uWS::WebSocket<uWS::SERVER> socket, char *message, size_t length, uWS::OpCode opCode);

  int port;
  uWS::Hub hub;
};

