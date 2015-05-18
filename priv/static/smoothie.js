var Smoothie = new (function() {
  this.connect = function(options) {
    return new SmoothieWebSocket(options);
  }
})();

/**
 * Usage example:
 * 
 * new SmoothieWebSocket({
 *   protocol:     "ws://",
 *   host:         "example.com",
 *   port:         3000,
 *   path:         "/ws"
 *   onOpen:       function() {},
 *   onMessage:    function(msg) {},
 *   onDisconnect: function() {},
 *   onClose:      function() {}
 * }
 */
function SmoothieWebSocket(options) {
  this.options    = options;
  this.connection = undefined;
  
  var active = false;
  
  var getOption = function(name, defaultValue) {
    return this.options[name] ? this.options[name] : defaultValue;
  }
  
  var init = function(options) {
    var protocol = getOption("protocol", window.location.protocol == "https:" ? "wss://" : "ws://");
    var host     = getOption("host", location.hostname);
    var port     = getOption("port", location.port);
    var path     = getOption("path", "/ws");
    var url      = protocol + host + ":" + port + path;
    
    this.connection              = $.bullet(url);
    this.connection.onopen       = onOpen;
    this.connection.onheartbeat  = onHeartBeat;
    this.connection.onmessage    = onMessage;
    this.connection.ondisconnect = onDisconnect;
    this.connection.onclose      = onClose;
  }
  
  this.send = function(data) {
    if (!active) {
      return false;
    }
    
    this.connection.send(data);
  }
  
  var callHandler = function(name, args) {
    if (this.options[name]) {
      this.options[name].apply(null, args);
    }
  }
  
  var onOpen = function() {
    console.log('SmoothieWebSocket: opened');
    active = true;
    this.callHandler("onOpen");
  }
  
  var onHeartBeat = function() {
    console.log('SmoothieWebSocket: heartbeat recieved');
    this.connection.send('ping');
  }
  
  var onMessage = function(msg) {
    console.log('SmoothieWebSocket: recieved message');
    console.log(msg);
    this.callHandler("onMessage", [msg]);
  }
  
  var onDisconnect = function() {
    console.log('SmoothieWebSocket: disconnected');
    active = false;
    this.callHandler("onDisconnect");
  }
  
  var onClose = function() {
    console.log('SmoothieWebSocket: closed');
    active = false;
    this.callHandler("onClose");
  }
  
  init(this.options);
}