package app;

import io.undertow.Undertow;
import io.undertow.websockets.WebSocketConnectionCallback;
import io.undertow.websockets.core.AbstractReceiveListener;
import io.undertow.websockets.core.BufferedTextMessage;
import io.undertow.websockets.core.WebSocketChannel;
import io.undertow.websockets.core.WebSockets;
import io.undertow.websockets.spi.WebSocketHttpExchange;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;
import org.xnio.ChannelListener;

import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import static io.undertow.Handlers.path;
import static io.undertow.Handlers.websocket;

public class WebSocketServer {

    public static final Queue<WebSocketChannel> channels = new ConcurrentLinkedQueue<>();

    public static void main(final String[] args) {
        Undertow server = Undertow.builder()
            .addHttpListener(3030, "0.0.0.0")
            .setHandler(path()
                .addPrefixPath("/ws", websocket(new WebSocketConnectionCallback() {
                    @Override
                    public void onConnect(WebSocketHttpExchange exchange, WebSocketChannel channel) {
                        channels.add(channel);
                        channel.getReceiveSetter().set(new AbstractReceiveListener() {
                            @Override
                            protected void onFullTextMessage(WebSocketChannel channel, BufferedTextMessage message) {
                                JSONObject msg = (JSONObject) JSONValue.parse(message.getData());

                                switch (msg.get("type").toString()) {
                                    case "broadcast":
                                        final String outbound = msg.toJSONString();
                                        channels.forEach(gc -> WebSockets.sendText(outbound, gc , null));
                                        msg.replace("type", "broadcastResult");
                                        WebSockets.sendText(msg.toJSONString(), channel, null);
                                        break;
                                    case "echo":
                                        WebSockets.sendText(msg.toJSONString(), channel, null);
                                        break;
                                    default:
                                        System.out.println("Unknown message type");
                                }
                            }
                        });

                        channel.addCloseTask(new ChannelListener<WebSocketChannel>() {
                            @Override
                            public void handleEvent(WebSocketChannel channel) {
                                channels.remove(channel);
                            }
                        });
                        channel.resumeReceives();
                    }
                })))
            .build();
        server.start();
    }

}
