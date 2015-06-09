var Smoothie = (function (erl) {
    // protocols
    var SmoothieProtocols = [];
    SmoothieProtocols['bert'] = (function () {
        return {
            encode: function (data) { return erl.encode(data) },
            decode: function (data) { return erl.decode(data) }
        }
    })();
    SmoothieProtocols['relay'] = (function () {
        return {
            encode: function (data) { return data },
            decode: function (data) { return data }
        }
    })();

    //public methods
    return {
        connect: newWebSocketConnection
    };

    // create new ws connection
    function newWebSocketConnection(options){
        var webSocket = createWebSocket(getUrl(options));
        if(webSocket){
            // create new connection with handlers from options
            return new WebSocketConnection(webSocket, options);
        } else {
            console.log("Unable to instantiate WebSocket");
        }
    }

    function getUrl(options){
        var http            = options["http"] || {};
        var defaultProtocol = window.location.protocol == "https:" ? "wss://" : "ws://";
        var protocol        = http["protocol"] || defaultProtocol;
        var host            = http["host"] || location.hostname;
        var port            = http["port"] || location.port;
        var path            = http["path"] || "/ws";

        return protocol + host + ":" + port + path;
    }

    function createWebSocket(url) {
        if (window.WebSocket) {
            return new window.WebSocket(url);
        } else if (window.MozWebSocket && navigator.userAgent.indexOf("Firefox/6.0") == -1) {
            return new window.MozWebSocket(url);
        } else {
            return null;
        }
    }

    function WebSocketConnection(webSocket, options){
        var active = null;
        var heartBeatTimeout = null;

        webSocket.binaryType   = "arraybuffer";
        webSocket.onopen       = onOpen;
        webSocket.onmessage    = onMessage;
        webSocket.ondisconnect = onDisconnect;
        webSocket.onclose      = onClose;

        heartBeatTimeout = setInterval(onHeartBeat, options.heartbeatDelay || 20000);

        return {
            send: send,
            close: close
        };

        function send(data){
            if (!active) {
                return false;
            }

            if (options["onBeforeSend"]) {
                data = options["onBeforeSend"](data);
                if (data === false) {
                    return false;
                }
            }

            var protocol = getProtocol();
            var encoded  = protocol.encode(data);

            webSocket.send(encoded);
        }

        function close(){
            webSocket.close();
        }

        function getProtocol() {
            var protocol     = options.protocol || "relay";
            return SmoothieProtocols[protocol];
        }

        function callHandler(name, args) {
            if (options[name]) {
                options[name].apply(null, args);
            }
        }

        function onOpen() {
            console.log("SmoothieWebSocket: opened");
            active = true;
            callHandler("onOpen");
        }

        function onHeartBeat() {
            webSocket.send("ping");
        }

        function onMessage(msg) {
            if (msg.data != "pong") {
                var protocol = getProtocol();
                var decoded  = protocol.decode(msg.data);

                callHandler("onMessage", [decoded]);
            }
        }

        function onDisconnect() {
            console.log("SmoothieWebSocket: disconnected");
            active = false;
            callHandler("onDisconnect");
        }

        function onClose() {
            console.log("SmoothieWebSocket: closed");
            clearInterval(heartBeatTimeout);
            active = false;
            callHandler("onClose");
        }

    }

})(Erl);
