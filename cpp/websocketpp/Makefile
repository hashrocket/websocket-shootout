bin/cpp-websocketpp-server : src/*
	g++ -std=c++14 -O2 -Ivendor/jsoncpp/include src/*.cpp vendor/jsoncpp/src/jsoncpp.cpp -lboost_system -lboost_thread -o bin/cpp-websocketpp-server

.PHONY : clean
clean :
	rm -f bin/*
