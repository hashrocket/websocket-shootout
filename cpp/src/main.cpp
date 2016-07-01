#include <iostream>
#include <string>
#include <memory>

#include <tclap/CmdLine.h>

#include <websocketpp/config/asio_no_tls.hpp>

#include <websocketpp/server.hpp>

typedef websocketpp::server<websocketpp::config::asio> server;

using websocketpp::lib::placeholders::_1;
using websocketpp::lib::placeholders::_2;
using websocketpp::lib::bind;

// pull out the type of messages sent by our config
typedef server::message_ptr message_ptr;

struct cliOptions {
	std::string address;
	int port;
};

// Define a callback to handle incoming messages
void on_message(server* s, websocketpp::connection_hdl hdl, message_ptr msg) {
	std::cout << "on_message called with hdl: " << hdl.lock().get()
		<< " and message: " << msg->get_payload()
		<< std::endl;

	// check for a special command to instruct the server to stop listening so
	// it can be cleanly exited.
	if (msg->get_payload() == "stop-listening") {
		s->stop_listening();
		return;
	}

	try {
		s->send(hdl, msg->get_payload(), msg->get_opcode());
	}
	catch (const websocketpp::lib::error_code& e) {
		std::cout << "Echo failed because: " << e
			<< "(" << e.message() << ")" << std::endl;
	}
}

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

	std::cout << "Hello, world\n";

	// Create a server endpoint
	server echo_server;

	try {
		// Set logging settings
		echo_server.set_access_channels(websocketpp::log::alevel::all);
		echo_server.clear_access_channels(websocketpp::log::alevel::frame_payload);

		// Initialize Asio
		echo_server.init_asio();

		// Register our message handler
		echo_server.set_message_handler(bind(&on_message, &echo_server, ::_1, ::_2));

		boost::asio::ip::tcp::endpoint ep(
			boost::asio::ip::address::from_string(cliOptions->address),
			cliOptions->port
		);
		echo_server.listen(ep);

		// Start the server accept loop
		echo_server.start_accept();

		// Start the ASIO io_service run loop
		echo_server.run();
	}
	catch (websocketpp::exception const & e) {
		std::cout << e.what() << std::endl;
	}
	catch (...) {
		std::cout << "other exception" << std::endl;
	}

	return 0;
}


