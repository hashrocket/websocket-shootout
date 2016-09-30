#pragma once

#include <vector>
#include <mutex>

#include <uWS.h>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>

class server {
public:
  server(int port, int threadCount);
  void run();

private:
  void echo(uWS::WebSocket socket, rapidjson::Value& payloadVal);
  void broadcast(uWS::WebSocket socket, rapidjson::Value& payloadVal);

  void onMessage(uWS::WebSocket socket, char *message, size_t length, uWS::OpCode opCode);
  void onUpgrade(uv_os_fd_t fd, const char *secKey, void *ssl, const char *extensions, size_t extensionsLength);

  int port;
  uWS::EventSystem uwsES;
  uWS::Server uwsServer;
  std::vector<uWS::Server*> threadServers;
  std::vector<std::unique_ptr<std::mutex>> threadServerMutexes;
};

