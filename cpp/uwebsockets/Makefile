bin/cpp-uwebsockets-server : src/*
	g++ -std=c++14 -O2 -Ivendor/uWebSockets/src -Ivendor/rapidjson/include src/*.cpp -L$(shell pwd)/vendor/uWebSockets -lpthread -luWS -Wl,-rpath,$(shell pwd)/vendor/uWebSockets -o bin/cpp-uwebsockets-server

.PHONY : clean
clean :
	rm -f bin/*
