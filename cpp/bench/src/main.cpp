// Highly modified from https://github.com/uWebSockets/uWebSockets/blob/master/benchmarks/throughput.cpp
// to implement the same binary protocol as the Go benchmark does for broadcast.
// License: https://github.com/uWebSockets/uWebSockets/blob/master/LICENSE

#include <iostream>
#include <vector>
#include <algorithm>
#include <chrono>
#include <sstream>
#include <uv.h>
#include <cstring>
#include <endian.h>
#include <tclap/CmdLine.h>
using namespace std;
using namespace chrono;

void broadcast();
void onConnect(uv_connect_t *connect, int status);
void onRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf);
void newConnection();
void startSample();

struct cliOptions {
	int payloadPadding;
	string address;
	int port;
};

int concurrent;
int sampleSize;
int stepSize;
int initialClients;
string path;

std::unique_ptr<cliOptions> parseCLI(int argc, const char * argv[]) {
	TCLAP::CmdLine cmd("cpp-bench", ' ', "0.1");

	TCLAP::ValueArg<int> concurrentArg("c", "concurrent", "concurrent broadcast requests", false, 4, "int");
	cmd.add(concurrentArg);

	TCLAP::ValueArg<int> payloadPaddingArg("", "payload-padding", "payload padding size", false, 0, "int");
	cmd.add(payloadPaddingArg);

	TCLAP::ValueArg<int> sampleSizeArg("s", "sample-size", "number of broadcasts in a sample", false, 100, "int");
	cmd.add(sampleSizeArg);

	TCLAP::ValueArg<int> stepSizeArg("", "step-size", "number of clients to increase each step", false, 1000, "int");
	cmd.add(stepSizeArg);

	TCLAP::ValueArg<int> initialClientsArg("", "initial-clients", "initial number of clients", false, 0, "int");
	cmd.add(initialClientsArg);

	TCLAP::ValueArg<string> addressArg("a", "address", "IP address", false, "127.0.0.1", "string");
	cmd.add(addressArg);

	TCLAP::ValueArg<int> portArg("p", "port", "port", false, 3000, "int");
	cmd.add(portArg);

	TCLAP::ValueArg<string> pathArg("", "path", "path to request", false, "/", "string");
	cmd.add(pathArg);

	cmd.parse(argc, argv);

	auto options = std::make_unique<cliOptions>();
	concurrent = concurrentArg.getValue();
	options->payloadPadding = payloadPaddingArg.getValue();
	sampleSize = sampleSizeArg.getValue();
	stepSize = stepSizeArg.getValue();
	initialClients = initialClientsArg.getValue();
	path = pathArg.getValue();
	options->address = addressArg.getValue();
	options->port = portArg.getValue();

	return options;
}

struct socketData {
	int remainingBytes;
	uint64_t broadcastID;
};

struct broadcastData {
	time_point<high_resolution_clock> beginTime;
	milliseconds rtt;
	int rxCount;
};

struct sampleSet {
	vector<broadcastData> results;
	int inProgressCount;
};

uv_loop_t *loop;
uv_buf_t upgradeHeader;
uv_buf_t broadcastTemplate;
int typeOffset, broadcastIDOffset;

sampleSet currentSampleSet;
int desiredConnectionCount;
vector<uv_stream_t *> sockets;
sockaddr_in addr;

uv_buf_t buildUpgradeHeader(std::string host, std::string path) {
	std::stringstream ss;
	ss
		<< "GET " << path << " HTTP/1.1\r\n"
		<< "Host: " << host << "\r\n"
		<< "User-Agent: bench\r\n"
		<< "Connection: Upgrade\r\n"
		<< "Sec-WebSocket-Key: +flVmtyN/Gkf21sc7WUZ5g==\r\n"
		<< "Sec-WebSocket-Version: 13\r\n"
		<< "Upgrade: websocket\r\n"
		<< "\r\n";

	uv_buf_t buf;
	auto size = ss.str().size();
	buf.base = new char[size];
	std::memcpy(buf.base, ss.str().data(), size);
	buf.len = size;

	return buf;
}

void buildBroadcastTemplate(int payloadPadding) {
	int broadcastMsgSize = 13 + payloadPadding; // 1 byte msg type + 4 byte app payload size + 8 byte broadcast ID
	uv_buf_t buf;

	int allocLength = broadcastMsgSize + 14;
	buf.base = new char[allocLength];
	for (int i = 0; i < allocLength; i++) {
		buf.base[i] = i % 255;
	}

	auto writePtr = buf.base;

	// First byte of frame header
	*writePtr = 130;
	writePtr++;

	if (broadcastMsgSize < 126) {
		*writePtr = 128 | broadcastMsgSize;
		writePtr++;
	} else if (broadcastMsgSize <= UINT16_MAX) {
		// Two byte size
		*writePtr = 128 | 126;
		writePtr++;

		*((uint16_t *) writePtr) = htons(broadcastMsgSize);
		writePtr += sizeof(uint16_t);
	} else {
		// 8 byte size
		*writePtr = 128 | 127;
		*writePtr++;

		*((uint64_t *) writePtr) = htobe64(broadcastMsgSize);
		writePtr += sizeof(uint64_t);
	}

	// Effectively disable masking
	*((uint32_t *) writePtr) = 0;
	writePtr += sizeof(uint32_t);

	// broadcast message type
	typeOffset = writePtr - buf.base;
	*writePtr = 'b';
	writePtr++;

	// size of payload
	*((uint32_t *) writePtr) = htonl(payloadPadding + 8);
	writePtr += sizeof(uint32_t);

	broadcastIDOffset = writePtr - buf.base;
	writePtr += 8;

	writePtr += payloadPadding;

	buf.len = writePtr - buf.base;

	broadcastTemplate = buf;
}

void onBroadcastWrite(uv_write_t *write_t, int status) {
	if (status < 0) {
		cout << "Write error" << endl;
		exit(0);
	}
	delete write_t;

	if (currentSampleSet.inProgressCount < concurrent && currentSampleSet.results.size() < sampleSize) {
		broadcast();
	}
}

void broadcast() {
	*((uint64_t*) (broadcastTemplate.base + broadcastIDOffset)) = htobe64(currentSampleSet.results.size());

	broadcastData bd;
	bd.beginTime = high_resolution_clock::now();
	bd.rxCount = 0;
	currentSampleSet.results.push_back(bd);
	currentSampleSet.inProgressCount++;

	// send broadcast request on random socket
	auto socket = sockets[rand() % sockets.size()];
	uv_write(new uv_write_t, socket, &broadcastTemplate, 1, onBroadcastWrite);
}

void onAlloc(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
	buf->base = new char[suggested_size];
	buf->len = suggested_size;
}

void onConnect(uv_connect_t *connect, int status) {
	if (status < 0) {
		cout << "Connection error" << endl;
		exit(-1);
	} else {
		uv_read_start(connect->handle, onAlloc, onRead);

		// Send upgrade header
		uv_write(new uv_write_t, connect->handle, &upgradeHeader, 1, [](uv_write_t *write_t, int status) {
			if (status < 0) {
				cout << "Connection error" << endl;
				exit(-1);
			}
			delete write_t;
		});
	}
}

void onRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
	if (stream->data) {
		auto expectedMsgSize = broadcastTemplate.len - 4; // no mask on frames from server
		if (nread % expectedMsgSize != 0) {
			cout << "bench cannot handle fragmented messages" << endl;
			exit(1);
		}

		// multiple messages may be bundled in one read
		int msgCount = nread / expectedMsgSize;
		char* readPtr = buf->base;
		for (int i = 0; i < msgCount; i++) {
			auto msgType = readPtr[typeOffset-4]; // -4 because no mask on server to client

			if (msgType == 'b') {
				uint64_t broadcastID = be64toh(*((uint64_t*) (readPtr + broadcastIDOffset - 4))); // -4 because no mask on server to client
				auto& result = currentSampleSet.results[broadcastID];
				result.rxCount++;
				if (result.rxCount == desiredConnectionCount) {
					result.rtt = duration_cast<milliseconds>(high_resolution_clock::now() - result.beginTime);

					currentSampleSet.inProgressCount--;
					if (currentSampleSet.results.size() < sampleSize) {
						broadcast();
					} else if (currentSampleSet.inProgressCount == 0) {
						sort(currentSampleSet.results.begin(), currentSampleSet.results.end(),
							[](broadcastData const& a, broadcastData const& b) { return a.rtt < b.rtt; });

						auto minRTT = currentSampleSet.results.front().rtt.count();
						auto medianRTT = currentSampleSet.results[currentSampleSet.results.size() / 2].rtt.count();
						auto perc95RTT = currentSampleSet.results[95 * currentSampleSet.results.size() / 100].rtt.count();
						auto maxRTT = currentSampleSet.results.back().rtt.count();

						if (perc95RTT > 500) {
							exit(0);
						}

						cout << "clients: " << desiredConnectionCount
							<< "    " << "95per-rtt: " << perc95RTT << "ms"
							<< "    " << "min-rtt: " << minRTT << "ms"
							<< "    " << "median-rtt: " << medianRTT << "ms"
							<< "    " << "max-rtt: " << maxRTT << "ms"
							<< endl;

						desiredConnectionCount += stepSize;
						newConnection();
					}
				}
			} else if (msgType == 'r') {
				// ignore broadcast result message as we will count broadcasts directly to determine when completed
			} else {
				cout << "received unknown message type: " << msgType << endl;
				exit(1);
			}

			readPtr += expectedMsgSize;
		}
	} else {
		// WebSocket connection established here
		auto data = new socketData;
		data->remainingBytes = 0;
		data->broadcastID = 0;
		stream->data = (void *) data;
		sockets.push_back(stream);

		if (sockets.size() == desiredConnectionCount) {
			startSample();
		} else {
			newConnection();
		}
	}
	delete [] buf->base;
}

void newConnection()
{
	uv_tcp_t *socket = new uv_tcp_t;
	socket->data = nullptr;
	uv_tcp_init(loop, socket);

	uv_tcp_connect(new uv_connect_t, socket, (sockaddr *) &addr, onConnect);
}

void startSample() {
	currentSampleSet.results.clear();
	currentSampleSet.inProgressCount = 0;
	broadcast();
}

int main(int argc, const char * argv[])
{
  std::unique_ptr<cliOptions> cliOptions;
  try {
    cliOptions = parseCLI(argc, argv);
  }
  catch (exception& e)
  {
    cout << e.what() << '\n';
    return 1;
  }

  currentSampleSet.inProgressCount = 0;
  if (initialClients > 0) {
		desiredConnectionCount = initialClients;
  } else {
		desiredConnectionCount = stepSize;
  }

	// Init
	loop = uv_default_loop();
	uv_ip4_addr(cliOptions->address.c_str(), cliOptions->port, &addr);
	upgradeHeader = buildUpgradeHeader("example.com", path);

	buildBroadcastTemplate(cliOptions->payloadPadding);

	newConnection();
	return uv_run(loop, UV_RUN_DEFAULT);
}

