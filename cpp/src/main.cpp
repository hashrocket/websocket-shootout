#include <iostream>
#include <string>
#include <sstream>
#include <memory>
#include <unordered_set>

#include <tclap/CmdLine.h>

#include <websocketpp/config/asio_no_tls.hpp>

#include <websocketpp/server.hpp>

#include "server.h"

struct cliOptions {
	std::string address;
	int port;
};

std::unique_ptr<cliOptions> parseCLI(int argc, const char * argv[]) {
	TCLAP::CmdLine cmd("cpp-websocket-server", ' ', "0.1");
	TCLAP::ValueArg<std::string> addressArg("a", "address", "address to listen on", false, "127.0.0.1", "string");
	cmd.add(addressArg);
	TCLAP::ValueArg<int> portArg("", "port", "port to listen on", false, 3000, "int");
	cmd.add(portArg);

	cmd.parse(argc, argv);

	auto options = std::make_unique<cliOptions>();
	options->address = addressArg.getValue();
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
		boost::asio::ip::tcp::endpoint ep(
			boost::asio::ip::address::from_string(cliOptions->address),
			cliOptions->port
		);
		server s(ep);
		s.run();
	}
	catch (websocketpp::exception const & e) {
		std::cout << e.what() << std::endl;
	}
	catch (...) {
		std::cout << "other exception" << std::endl;
	}

	return 0;
}


