require 'bundler/setup'
require_relative 'server'

Faye::WebSocket.load_adapter('thin')

run Server.new
