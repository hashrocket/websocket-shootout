require 'bundler/setup'
require_relative 'server'

Faye::WebSocket.load_adapter('puma')

run Server.new
