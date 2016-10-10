bin/cpp-crow-server : src/
	g++ -std=c++14 -O2 -Ivendor/crow/include -I../vendor/rapidjson/include -L /usr/lib/x86_64-linux-gnu src/*.cpp -lstdc++ -lboost_system -lboost_thread -lpthread -Wl,-rpath -o bin/cpp-crow-server

.PHONY : clean
clean :
	rm -f bin/*
