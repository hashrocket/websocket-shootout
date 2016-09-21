#include <iostream>
#include <string>
#include <sstream>
#include <memory>
#include <unordered_set>

#include <tclap/CmdLine.h>

#include "server.h"

struct cliOptions {
  int port;
};

std::unique_ptr<cliOptions> parseCLI(int argc, const char * argv[]) {
  TCLAP::CmdLine cmd("cpp-uwebsockets-server", ' ', "0.1");
  TCLAP::ValueArg<int> portArg("p", "port", "port to listen on", false, 3000, "int");
  cmd.add(portArg);

  cmd.parse(argc, argv);

  auto options = std::make_unique<cliOptions>();
  options->port = portArg.getValue();

  return options;
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
    server s(cliOptions->port);
    s.run();
  }
  catch (...) {
    std::cout << "something bad happened" << std::endl;
  }

  return 0;
}


