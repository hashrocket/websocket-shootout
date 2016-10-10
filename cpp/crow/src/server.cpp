#include "crow.h"
#include "shared_mutex.hpp"

#include <unordered_set>
#include <mutex>
#include <iostream>
#include <string>
#include <sstream>
#include <memory>
#include <unordered_set>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>

#include <tclap/CmdLine.h>

struct cliOptions {
  int port;
  int threadCount;
};

std::unique_ptr<cliOptions> parseCLI(int argc, const char * argv[]) {
  TCLAP::CmdLine cmd("cpp-uwebsockets-server", ' ', "0.1");
  TCLAP::ValueArg<int> portArg("p", "port", "port to listen on", false, 3000, "int");
  cmd.add(portArg);
  TCLAP::ValueArg<int> threadCountArg("", "thread", "number of threads", false, 8, "int");
  cmd.add(threadCountArg);

  cmd.parse(argc, argv);

  auto options = std::make_unique<cliOptions>();
  options->port = portArg.getValue();
  options->threadCount = threadCountArg.getValue();

  return options;
}

int run_server(int port, int threadCount)
{
  crow::SimpleApp app;

  shared_mutex mtx;
  std::unordered_set<crow::websocket::connection*> users;

  CROW_ROUTE(app, "/")
  .websocket()
  .onopen([&](crow::websocket::connection& conn){
    mtx.lock();
    users.insert(&conn);
    mtx.unlock();
  })
  .onclose([&](crow::websocket::connection& conn, const std::string& reason){
    mtx.lock();
    users.erase(&conn);
    mtx.unlock();
  })
  .onmessage([&](crow::websocket::connection& conn, const std::string& data, bool is_binary){
    if (is_binary) {
      CROW_LOG_ERROR << "Unable to parse message";
      return;
    }

    rapidjson::Document doc;
    doc.Parse(data.c_str(), data.length());
    if (doc.HasParseError()) {
      CROW_LOG_ERROR << "Unable to parse message";
      return;
    }

    rapidjson::Value& typeVal = doc["type"];
    rapidjson::Value& payloadVal = doc["payload"];

    auto type = std::string(typeVal.GetString());
    if (type == "echo") {
      conn.send_text(data);
    }
    else if (type == "broadcast") {
      rapidjson::Document broadcastDoc;
      broadcastDoc.SetObject();
      broadcastDoc.AddMember("type", "broadcast", broadcastDoc.GetAllocator());
      broadcastDoc.AddMember("payload", payloadVal, broadcastDoc.GetAllocator());

      rapidjson::StringBuffer broadcastBuffer;
      rapidjson::Writer<rapidjson::StringBuffer> broadcastWriter(broadcastBuffer);
      broadcastDoc.Accept(broadcastWriter);
      auto broadcastData = broadcastBuffer.GetString();

      rapidjson::Document resultDoc;
      resultDoc.SetObject();
      resultDoc.AddMember("type", "broadcastResult", resultDoc.GetAllocator());
      resultDoc.AddMember("payload", broadcastDoc["payload"], resultDoc.GetAllocator());

      rapidjson::StringBuffer resultBuffer;
      rapidjson::Writer<rapidjson::StringBuffer> resultWriter(resultBuffer);
      resultDoc.Accept(resultWriter);

      mtx.lock_shared();
      for(auto u:users) {
        u->send_text(broadcastData);
      }
      mtx.unlock_shared();

      conn.send_text(resultBuffer.GetString());
    }
    else {
      CROW_LOG_ERROR << "bad type";
    }
  });

  app.port(port)
    .concurrency(threadCount)
    .run();
}

int main(int argc, const char * argv[]) {
  std::unique_ptr<cliOptions> cliOptions;
  try {
    cliOptions = parseCLI(argc, argv);
  }
  catch (...) {
    std::cerr << "error parsing arguments";
    return 1;
  }

  try {
    run_server(cliOptions->port, cliOptions->threadCount);
  }
  catch (...) {
    std::cout << "something bad happened" << std::endl;
  }

  return 0;
}
