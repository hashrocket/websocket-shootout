/*
 * JBoss, Home of Professional Open Source.
 * Copyright 2014 Red Hat, Inc., and individual contributors
 * as indicated by the @author tags.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

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

/**
 * @author Stuart Douglas
 */
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
