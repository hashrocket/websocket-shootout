#include <iostream>
#include <string>
#include <sstream>
#include <memory>
#include <unordered_set>

#include <tclap/CmdLine.h>

#include "server.h"
#include "binary_server.h"

struct cliOptions {
  int port;
  std::string dataType;
};

std::unique_ptr<cliOptions> parseCLI(int argc, const char * argv[]) {
  TCLAP::CmdLine cmd("cpp-uwebsockets-server", ' ', "0.3");
  TCLAP::ValueArg<int> portArg("p", "port", "port to listen on", false, 3000, "int");
  cmd.add(portArg);
  TCLAP::ValueArg<std::string> dataTypeArg("t", "data-type", "date type (json, binary)", false, "json", "string");
  cmd.add(dataTypeArg);

  cmd.parse(argc, argv);

  auto options = std::make_unique<cliOptions>();
  options->port = portArg.getValue();
  options->dataType = dataTypeArg.getValue();

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

    if (cliOptions->dataType == "json") {
      server s(cliOptions->port);
      if (!s.run()) {
        std::cout << "Failed to listen on port " << cliOptions->port << ". Is it already in use?" << std::endl;
        return 1;
      }
    } else if (cliOptions->dataType == "binary") {
      binary_server s(cliOptions->port);
      if (!s.run()) {
        std::cout << "Failed to listen on port " << cliOptions->port << ". Is it already in use?" << std::endl;
        return 1;
      }
    } else {
      std::cout << "unknown data type: " << cliOptions->dataType << std::endl;
    }




  return 0;
}


