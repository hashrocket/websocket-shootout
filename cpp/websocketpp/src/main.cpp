#include <iostream>
#include <string>
#include <sstream>
#include <memory>
#include <unordered_set>

#include <tclap/CmdLine.h>

#include <websocketpp/config/asio_no_tls.hpp>

#include <websocketpp/server.hpp>

#include "json_server.h"
#include "binary_server.h"

struct cliOptions {
	std::string address;
	std::string dataType;
	int port;
	int threadCount;
};

std::unique_ptr<cliOptions> parseCLI(int argc, const char * argv[]) {
	TCLAP::CmdLine cmd("cpp-websocket-server", ' ', "0.2");
	TCLAP::ValueArg<std::string> addressArg("a", "address", "address to listen on", false, "127.0.0.1", "string");
	cmd.add(addressArg);
	TCLAP::ValueArg<std::string> dataTypeArg("t", "data-type", "date type (json, binary)", false, "json", "string");
	cmd.add(dataTypeArg);
	TCLAP::ValueArg<int> portArg("", "port", "port to listen on", false, 3000, "int");
	cmd.add(portArg);
	TCLAP::ValueArg<int> threadCountArg("", "thread", "number of threads", false, 8, "int");
	cmd.add(threadCountArg);

	cmd.parse(argc, argv);

	auto options = std::make_unique<cliOptions>();
	options->address = addressArg.getValue();
	options->dataType = dataTypeArg.getValue();
	options->port = portArg.getValue();
	options->threadCount = threadCountArg.getValue();

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
		boost::asio::ip::tcp::endpoint ep(
			boost::asio::ip::address::from_string(cliOptions->address),
			cliOptions->port
		);

		if (cliOptions->dataType == "json") {
			server s(ep);
			s.run(cliOptions->threadCount);
		} else if (cliOptions->dataType == "binary") {
			binary_server s(ep);
			s.run(cliOptions->threadCount);
		} else {
			std::cout << "unknown data type: " << cliOptions->dataType << std::endl;
		}
	}
	catch (websocketpp::exception const & e) {
		std::cout << e.what() << std::endl;
	}
	catch (...) {
		std::cout << "other exception" << std::endl;
	}

	return 0;
}


