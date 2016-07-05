#pragma once

#include <memory>
#include <set>

#include <websocketpp/config/asio_no_tls.hpp>
#include <websocketpp/server.hpp>

#include <json/json.h>

class server {
public:
	server(boost::asio::ip::tcp::endpoint ep);
	void run();

private:
	void on_open(websocketpp::connection_hdl hdl);
	void on_close(websocketpp::connection_hdl hdl);
	void on_message(websocketpp::connection_hdl hdl, websocketpp::server<websocketpp::config::asio>::message_ptr msg);

	void echo(websocketpp::connection_hdl hdl, const Json::Value &msg);
	void broadcast(websocketpp::connection_hdl src_hdl, const Json::Value &src_msg);

	std::string json_to_string(Json::Value json);

	websocketpp::server<websocketpp::config::asio> wspp_server;
	std::set<websocketpp::connection_hdl, std::owner_less<websocketpp::connection_hdl>> conns;
};
