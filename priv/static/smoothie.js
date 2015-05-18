var Smoothie = new (function() {
  this.connect = function(options) {
    return new SmoothieWebSocket(options);
  }
})();

/**
 * Usage example:
 * 
 * new SmoothieWebSocket({
 *   protocol:       "ws://",
 *   host:           "example.com",
 *   port:           3000,
 *   path:           "/ws",
 *   heartBeatDelay: 20000,
 *
 *   onOpen:         function() {},
 *   onMessage:      function(msg) {},
 *   onDisconnect:   function() {},
 *   onClose:        function() {},
 *
 *   onBeforeSend:   function(data) {}
 * }
 */
function SmoothieWebSocket(options) {
  this.options    = options;
  this.connection = undefined;
  
  var that   = this;
  var active = false;
  var heartBeatTimeout;
  
  var getOption = function(name, defaultValue) {
    return that.options[name] ? that.options[name] : defaultValue;
  }
  
  var createWebSocket = function(url) {
    if (window.WebSocket) {
      return new window.WebSocket(url);
    }
    
    if (window.MozWebSocket && navigator.userAgent.indexOf("Firefox/6.0") == -1) {
      return new window.MozWebSocket(url);
    }
    
    return false;
  }
  
  var init = function(options) {
    var heartBeatDelay  = getOption("heartbeatDelay", 20000);
    var defaultProtocol = window.location.protocol == "https:" ? "wss://" : "ws://";
    var protocol        = getOption("protocol", defaultProtocol);
    var host            = getOption("host", location.hostname);
    var port            = getOption("port", location.port);
    var path            = getOption("path", "/ws");
    var url             = protocol + host + ":" + port + path;
    
    that.connection = createWebSocket(url);
    if (that.connection === false) {
      console.log("Unable to instantiate WebSocket");
      return;
    }

    that.connection.onopen       = onOpen;
    that.connection.onmessage    = onMessage;
    that.connection.ondisconnect = onDisconnect;
    that.connection.onclose      = onClose;

    heartBeatTimeout = setInterval(onHeartBeat, 20000);
  }

  this.send = function(data) {
    if (!active) {
      return false;
    }

    if (this.options["onBeforeSend"]) {
      data = this.options["onBeforeSend"](data);
      if (data === false) {
        return false;
      }
    }

    this.connection.send(data);
  }
  
  var callHandler = function(name, args) {
    if (that.options[name]) {
      that.options[name].apply(null, args);
    }
  }
  
  var onOpen = function() {
    console.log("SmoothieWebSocket: opened");
    active = true;
    callHandler("onOpen");
  }
  
  var onHeartBeat = function() {
//    console.log("SmoothieWebSocket: heartbeat recieved");
    that.connection.send("ping");
  }
  
  var onMessage = function(msg) {
//    console.log("SmoothieWebSocket: recieved message", msg);
    if (msg.data != "pong") {
      callHandler("onMessage", [msg]);
    }
  }
  
  var onDisconnect = function() {
    console.log("SmoothieWebSocket: disconnected");
    active = false;
    callHandler("onDisconnect");
  }
  
  var onClose = function() {
    console.log("SmoothieWebSocket: closed");
    clearInterval(heartBeatTimeout);
    active = false;
    callHandler("onClose");
  }
  
  init(this.options);
}

var SmoothieBert = new (function(erl) {
  var bert = erl;

  this.encode = function(obj) {
    var buffer = bert.encode(obj);
    return buffer;
  }
  
  this.decode = function(data) {
    return bert.decode(data);
  }
})(Erl);