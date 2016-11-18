#pragma once

#include <uWS.h>

class binary_server {
public:
  binary_server(int port);
  bool run();

private:
  void echo(uWS::WebSocket<uWS::SERVER> socket, char *msg, size_t length);
  void broadcast(uWS::WebSocket<uWS::SERVER> socket, char *msg, size_t length);

  void onMessage(uWS::WebSocket<uWS::SERVER> socket, char *message, size_t length, uWS::OpCode opCode);

  int port;
  uWS::Hub hub;
};

