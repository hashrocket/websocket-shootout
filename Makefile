GOPATH=$(CURDIR)/go

.PHONY : all
all : bin/go-websocket-server bin/websocket-bench

bin/go-websocket-server : $(GOPATH)/bin/go-websocket-server
	cp $< $@

bin/websocket-bench : $(GOPATH)/bin/websocket-bench
	cp $< $@

$(GOPATH)/bin/go-websocket-server : $(GOPATH)/src/hashrocket/go-websocket-server/*.go
	cd go/src/hashrocket/go-websocket-server && go install

$(GOPATH)/bin/websocket-bench : $(GOPATH)/src/hashrocket/websocket-bench/*.go
	cd go/src/hashrocket/websocket-bench && go install

.PHONY : clean
clean :
	rm bin/*
	rm $(GOPATH)/bin/*
