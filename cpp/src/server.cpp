#include <iostream>

#include <json/json.h>

#include "server.h"

server::server(boost::asio::ip::tcp::endpoint ep) {
	using websocketpp::lib::placeholders::_1;
	using websocketpp::lib::placeholders::_2;
	using websocketpp::lib::bind;

	wspp_server.clear_access_channels(websocketpp::log::alevel::all);

	wspp_server.init_asio();

	wspp_server.set_open_handler(bind(&server::on_open, this, _1));
	wspp_server.set_close_handler(bind(&server::on_close, this, _1));
	wspp_server.set_message_handler(bind(&server::on_message, this, _1, _2));

	wspp_server.listen(ep);
}

void server::run() {
	wspp_server.start_accept();
	wspp_server.run();
}

void server::on_open(websocketpp::connection_hdl hdl) {
	conns.insert(hdl);
}

void server::on_close(websocketpp::connection_hdl hdl) {
	conns.erase(hdl);
}

void server::on_message(websocketpp::connection_hdl hdl, websocketpp::server<websocketpp::config::asio>::message_ptr msg) {
	Json::Value root;
	Json::Reader r;
	if (!r.parse(msg->get_payload(), root)) {
		std::cout << "Unable to parse JSON";
		return;
	}

	Json::Value response;
	try {
		auto type = root["type"].asString();
		if (type == "echo") {
			echo(hdl, root);
		}
		else if (type == "broadcast") {
			broadcast(hdl, root);
		}
		else {
			response = "bad type";
		}
	}
	catch (const websocketpp::lib::error_code& e) {
		std::cout << "Echo failed because: " << e
			<< "(" << e.message() << ")" << std::endl;
	}
}


void server::echo(websocketpp::connection_hdl hdl, const Json::Value &msg) {
	Json::Value dst_msg;
	dst_msg["type"] = "echo";
	dst_msg["payload"] = msg["payload"];
	wspp_server.send(hdl, json_to_string(dst_msg), websocketpp::frame::opcode::text);
}

void server::broadcast(websocketpp::connection_hdl src_hdl, const Json::Value &src_msg) {
	Json::Value dst_msg;
	dst_msg["type"] = "broadcast";
	dst_msg["payload"] = src_msg["payload"];
	auto dst_msg_str = json_to_string(dst_msg);

	for (auto hdl : conns) {
		wspp_server.send(hdl, dst_msg_str, websocketpp::frame::opcode::text);
	}

	Json::Value result_msg;
	result_msg["type"] = "broadcastResult";
	result_msg["payload"] = src_msg["payload"];
	result_msg["listenCount"] = int(conns.size());
	wspp_server.send(src_hdl, json_to_string(result_msg), websocketpp::frame::opcode::text);
}

std::string server::json_to_string(Json::Value json) {
	Json::StreamWriterBuilder builder;
	builder["commentStyle"] = "None";
	auto writer = builder.newStreamWriter();
	std::ostringstream os;
	writer->write(json, &os);
	return os.str();
}
