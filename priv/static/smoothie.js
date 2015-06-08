var Smoothie = (function (Erl) {
    // protocols
    var SmoothieProtocols = [];
    SmoothieProtocols['bert'] = (function () {
        return {
            encode: function (data) { return Erl.encode(data) },
            decode: function (data) { return Erl.decode(data) }
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
        connect: newWebSocketConnection,
        sendFiles: newSendFilesConnection
    };

    // create new ws connection
    function newWebSocketConnection(options) {
        var webSocket = createWebSocket(getUrl(options));
        if(webSocket){
            // create new connection with handlers from options
            return new WebSocketConnection(webSocket, options);
        } else {
            console.log("Unable to instantiate WebSocket");
        }
    }

    // create new ws connection for sending files
    function newSendFilesConnection(options, files, completeCb, progressCb, errorCb) {
        new SendFilesConnection(options, files, completeCb, progressCb, errorCb);
    }

    function getUrl(options){
        var http            = options["http"] || {};
        var defaultProtocol = window.location.protocol == "https:" ? "wss://" : "ws://";
        var protocol        = http["protocol"] || defaultProtocol;
        var host            = http["host"] || location.hostname;
        var port            = http["port"] || location.port;
        var path            = http["path"] || "/ws";
        var queryParamsObj  = options["queryParams"];
        var queryParamsArray     = [];

        if(queryParamsObj) {
            for (var key in queryParamsObj){
                if (queryParamsObj.hasOwnProperty(key)){
                    queryParamsArray.push(encodeURIComponent(key) + "=" + encodeURIComponent(queryParamsObj[key]));
                }
            }
            queryParamsArray.join('&');
        }

        return (queryParamsArray.length)
            ? protocol + host + ":" + port + path + '/?' + queryParamsArray.join('&')
            : protocol + host + ":" + port + path;

    }

    function getProtocol(options) {
        var protocol     = options.protocol || "relay";
        return SmoothieProtocols[protocol];
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

    function WebSocketConnection(webSocket, options) {
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

            var protocol = getProtocol(options);
            var encoded  = protocol.encode(data);

            webSocket.send(encoded);
        }

        function close(){
            webSocket.close();
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
                var protocol = getProtocol(options);
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

    function SendFilesConnection(options, files, completeCb, progressCb, errorCb) {
        var protocol = getProtocol(options);
        var maxFileSize = options["maxFileSize"] || 1024 * 1024 * 5; // 5Mb default limit

        for (var i = 0; i < files.length; i++) {
            loadFile(files[i]);
        }

        function loadFile(file) {
            options["queryParams"] = options["queryParams"] || {};
            options["queryParams"]["fileName"] = file.name;
            options["queryParams"]["fileSize"] = file.size;

            var webSocket = createWebSocket(getUrl(options));

            if(!webSocket){
                console.log("Unable to instantiate WebSocket");
                return;
                //throw new Error("Unable to instantiate WebSocket");
            }

            webSocket.binaryType = "arraybuffer";

            webSocket.onopen = function () {
                webSocket.send(protocol.encode({
                    event: "start"
                }));
            };

            webSocket.onmessage = function(event) {
                switch (event.data.event) {
                    case "ready":
                        processChunk();
                        break;
                    case "complete":
                        completeCb && completeCb({
                            detail: event.data.detail
                        });
                        webSocket.close();
                        break;
                    case "error":
                        errorCb && errorCb({
                            message: event.data.message
                        });
                        webSocket.close();
                        break;
                }
            };

            if (file.size > maxFileSize) {
                errorCb && errorCb({
                    file: file,
                    message: "Attempt by client to upload file exceeding the maximum file size"
                });
                return;
            }

            var reader = new FileReader(),
                offset = 0,
                chunkSize = options.chunkSize || 40960;

            if (chunkSize >= file.size || chunkSize <= 0) chunkSize = file.size;

            var sendFilePart = function (start, end, content) {
                var uintArr = new Uint8Array(content);
                content = uintArr.buffer;

                webSocket.send(protocol.encode({
                    event: "chunk",
                    start: start,
                    end: end,
                    content: Erl.bufferToArray(content) // TODO check
                }));
            };

            var sendFileDone = function () {
                webSocket.send(protocol.encode({event: "done"}));
            };

            var processChunk = function () {
                var chunk = file.slice(offset, Math.min(offset+chunkSize, file.size));
                reader.readAsArrayBuffer(chunk);
            };

            reader.onload = function (event) {
                // Transmit the newly loaded data to the server
                // and emit a client event
                var bytesLoaded = Math.min(offset+chunkSize, file.size);
                sendFilePart(offset, bytesLoaded, event.target.result);
                progressCb && progressCb({
                    file: file,
                    bytesLoaded: bytesLoaded
                });

                // Get ready to send the next chunk
                offset += chunkSize;
                if (offset < file.size) {
                    // Read in the next chunk
                    processChunk();
                }
                else {
                    // All done!
                    sendFileDone();
                }
            };

            reader.onerror = reader.onabort = function() {
                webSocket.send(protocol.encode({
                    event: "done",
                    interrupt: true
                }));
            }

        }

    }

})(Erl);
