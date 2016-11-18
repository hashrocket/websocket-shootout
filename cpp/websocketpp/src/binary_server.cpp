#include <iostream>

#include <json/json.h>

#include <boost/thread/shared_lock_guard.hpp>

#include "binary_server.h"

binary_server::binary_server(boost::asio::ip::tcp::endpoint ep) {
	using websocketpp::lib::placeholders::_1;
	using websocketpp::lib::placeholders::_2;
	using websocketpp::lib::bind;

	wspp_server.clear_access_channels(websocketpp::log::alevel::all);

	wspp_server.init_asio();

	wspp_server.set_open_handler(bind(&binary_server::on_open, this, _1));
	wspp_server.set_close_handler(bind(&binary_server::on_close, this, _1));
	wspp_server.set_message_handler(bind(&binary_server::on_message, this, _1, _2));

	wspp_server.listen(ep);
	wspp_server.start_accept();
}

void binary_server::run(int threadCount) {
	boost::thread_group tg;

	for (int i = 0; i < threadCount; i++) {
		tg.add_thread(new boost::thread(&websocketpp::server<websocketpp::config::asio>::run, &wspp_server));
	}

	tg.join_all();
}

void binary_server::on_open(websocketpp::connection_hdl hdl) {
	boost::lock_guard<boost::shared_mutex> lock(conns_mutex);
	conns.insert(hdl);
}

void binary_server::on_close(websocketpp::connection_hdl hdl) {
	boost::lock_guard<boost::shared_mutex> lock(conns_mutex);
	conns.erase(hdl);
}

void binary_server::on_message(websocketpp::connection_hdl hdl, websocketpp::server<websocketpp::config::asio>::message_ptr msg) {
	auto buf = msg->get_payload();

	try {
		auto type = buf[0];
		if (type == 'e') {
			echo(hdl, buf);
		}
		else if (type == 'b') {
			broadcast(hdl, buf);
		}
		else {
			std::cout << "bad type: " << type << std::endl;
		}
	}
	catch (const websocketpp::lib::error_code& e) {
		std::cout << "Failed because: " << e
			<< "(" << e.message() << ")" << std::endl;
	}
}


void binary_server::echo(websocketpp::connection_hdl hdl, const std::string &msg) {
	wspp_server.send(hdl, msg, websocketpp::frame::opcode::binary);
}

void binary_server::broadcast(websocketpp::connection_hdl src_hdl, const std::string &src_msg) {
	boost::shared_lock_guard<boost::shared_mutex> lock(conns_mutex);

	for (auto hdl : conns) {
		wspp_server.send(hdl, src_msg, websocketpp::frame::opcode::binary);
	}

	std::string result_msg = src_msg;
	result_msg.replace(0, 1, "r");
	wspp_server.send(src_hdl, result_msg, websocketpp::frame::opcode::binary);
}
