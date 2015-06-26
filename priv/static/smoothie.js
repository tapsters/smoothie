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
    if (webSocket) {
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

  function getUrl(options) {
    var http = options["http"] || {};
    var defaultProtocol = window.location.protocol == "https:" ? "wss://" : "ws://";
    var protocol = http["protocol"] || defaultProtocol;
    var host = http["host"] || location.hostname;
    var port = http["port"] || location.port;
    var path = http["path"] || "/ws";
    var queryParamsObj = options["queryParams"];
    var queryParamsArray = [];

    if (queryParamsObj) {
      for (var key in queryParamsObj) {
        if (queryParamsObj.hasOwnProperty(key)) {
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
    var protocol = options.protocol || "relay";
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

    webSocket.binaryType = "arraybuffer";
    webSocket.onopen = onOpen;
    webSocket.onmessage = onMessage;
    webSocket.ondisconnect = onDisconnect;
    webSocket.onclose = onClose;

    heartBeatTimeout = setInterval(onHeartBeat, options.heartbeatDelay || 20000);

    return {
      send: send,
      close: close
    };

    function send(data) {
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
      var encoded = protocol.encode(data);

      webSocket.send(encoded);
    }

    function close() {
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
        var decoded = protocol.decode(msg.data);

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
    var maxFileSize = options["maxFileSize"] || 1024 * 1024 * 5; // 5Mb default limit

    for (var i = 0; i < files.length; i++) {
      loadFile(files[i]);
    }

    function loadFile(file) {
      options["queryParams"] = options["queryParams"] || {};
      options["queryParams"]["name"] = file.name;
      options["queryParams"]["size"] = file.size;
      options["queryParams"]["ext"]  = file.name.split(".").pop();

      var webSocket = createWebSocket(getUrl(options));

      if (!webSocket) {
        console.log("Unable to instantiate WebSocket");
        return;
      }

      webSocket.binaryType = "arraybuffer";

      webSocket.onopen = function () {
        console.log("Connection start!!!");
      };

      webSocket.onmessage = function (event) {
        var message = Erl.decode(event.data);
        var status = arrayBufferToString(message.status.value);
        if (message.data) {
          var data = decodeURIComponent(arrayBufferToString(message.data.value)) || null;
        }

        switch (status) {
          case "ready":
            processChunk();
            break;
          case "complete":
            completeCb && completeCb({
              message: data
            });
            webSocket.close();
            break;
          case "error":
            errorCb && errorCb({
              message: data
            });
            webSocket.close();
            break;
        }
      };

      if (file.size > maxFileSize) {
        errorCb && errorCb({
          message: "Мaximum file size!!!"
        });
        return;
      }

      var reader = new FileReader(),
        offset = 0,
        chunkSize = options.chunkSize || 40960;

      if (chunkSize >= file.size || chunkSize <= 0) chunkSize = file.size;

      var sendFilePart = function (start, end, content) {
        webSocket.send(content);
      };

      var sendFileDone = function () {
        webSocket.send(stringToArrayBuffer("done"));
      };

      var processChunk = function () {
        var chunk = file.slice(offset, Math.min(offset + chunkSize, file.size));
        reader.readAsArrayBuffer(chunk);
      };

      reader.onload = function (event) {
        if (webSocket.OPEN !== webSocket.readyState) {
          reader.abort();
          console.log("WebSocket has been closed.");
          errorCb && errorCb({
            message: "WebSocket has been closed."
          });
          return;
        }

        var bytesLoaded = Math.min(offset + chunkSize, file.size);
        sendFilePart(offset, bytesLoaded, event.target.result);
        progressCb && progressCb({
          bytesLoaded: bytesLoaded
        });

        // check for next chunk
        offset += chunkSize;
        if (offset < file.size) {
          //next chunk
          processChunk();
        } else {
          sendFileDone();
        }
      };

      reader.onerror = reader.onabort = function () {
        console.log("FileReader error!!!");
        if (webSocket.OPEN === webSocket.readyState) {
          webSocket.close();
        }
      }

    }

    // util function
    function stringToArrayBuffer(str) {
      var buf = new ArrayBuffer(str.length);
      var bufView = new Uint8Array(buf);
      for (var i = 0, strLen = str.length; i < strLen; i++) {
        bufView[i] = str.charCodeAt(i);
      }
      return buf;
    }

    function arrayBufferToString(a) {
      return String.fromCharCode.apply(null, new Uint8Array(a));
    }

  }

})(Erl);
