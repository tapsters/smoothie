var Smoothie = new (function() {
  this.connect = function(options) {
    return new SmoothieWebSocket(options);
  }
})();

/**
 * Usage example:
 * 
 * new SmoothieWebSocket({
 *   http: {
 *       protocol:   "ws://",
 *       host:       "example.com",
 *       port:       3000,
 *       path:       "/ws",
 *   },
 *   heartBeatDelay: 20000,
 *   protocol:       "bert", //"relay"
 *
 *   onOpen:         function() {},
 *   onMessage:      function(data) {},
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
    var http            = getOption("http", {});
    var defaultProtocol = window.location.protocol == "https:" ? "wss://" : "ws://";
    var protocol        = http["protocol"] ? http["protocol"] : defaultProtocol;
    var host            = http["host"]     ? http["host"]     : location.hostname;
    var port            = http["port"]     ? http["port"]     : location.port;
    var path            = http["path"]     ? http["path"]     : "/ws";
    var url             = protocol + host + ":" + port + path;
    
    that.connection = createWebSocket(url);
    if (that.connection === false) {
      console.log("Unable to instantiate WebSocket");
      return;
    }

    that.connection.binaryType   = "arraybuffer";
    that.connection.onopen       = onOpen;
    that.connection.onmessage    = onMessage;
    that.connection.ondisconnect = onDisconnect;
    that.connection.onclose      = onClose;

    heartBeatTimeout = setInterval(onHeartBeat, 20000);
  }
  
  var ucFirst = function(str) {
    var f = str.charAt(0).toUpperCase();
    return f + str.substr(1);
  }
  
  var getProtocol = function() {
    var protocol     = getOption("protocol", "relay");
    var protocolName = "SmoothieProtocol" + ucFirst(protocol);

    return window[protocolName];
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
    
    var protocol = getProtocol();
    var encoded  = protocol.encode(data);

    this.connection.send(encoded);
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
      var protocol = getProtocol();
      var decoded  = protocol.decode(msg.data);
      
      callHandler("onMessage", [decoded]);
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

var SmoothieProtocolBert = new (function(erl) {
  var bert = erl;

  this.encode = function(data) {
    return bert.encode(data);
  }
  
  this.decode = function(data) {
    return bert.decode(data);
  }
})(Erl);

var SmoothieProtocolRelay = new (function() {
  this.encode = function(data) {
    return data;
  }
  
  this.decode = function(data) {
    return data;
  }
})();