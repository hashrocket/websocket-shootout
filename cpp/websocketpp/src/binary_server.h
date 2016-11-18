#pragma once

#include <memory>
#include <set>

#include <websocketpp/config/asio_no_tls.hpp>
#include <websocketpp/server.hpp>

#include <boost/thread/thread.hpp>

class binary_server {
public:
	binary_server(boost::asio::ip::tcp::endpoint ep);
	void run(int threadCount);

private:
	void on_open(websocketpp::connection_hdl hdl);
	void on_close(websocketpp::connection_hdl hdl);
	void on_message(websocketpp::connection_hdl hdl, websocketpp::server<websocketpp::config::asio>::message_ptr msg);

	void echo(websocketpp::connection_hdl hdl, const std::string &msg);
	void broadcast(websocketpp::connection_hdl src_hdl, const std::string &src_msg);

	websocketpp::server<websocketpp::config::asio> wspp_server;

	boost::shared_mutex conns_mutex;
	std::set<websocketpp::connection_hdl, std::owner_less<websocketpp::connection_hdl>> conns;
};
