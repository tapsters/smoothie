WebSocket File Upload Example
=============================

Server
------

Server code is located under `server_and_js_client`. Starting up:

```
cd server_and_js_client
mad deps compile plan repl
```

Default port is `5000`. You can specify port in `sys.config`.

JavaScript Client
-----------------

JavaScript client code is located under `server_and_js_client/apps/file_upload/priv/static/index.html`. 

To test file upload start server and open url:

[http://localhost:5000](http://localhost:5000)

Android Client
--------------

Android client code is located under `android_client`.

To test file upload start server and edit `org.smoothie.fileupload.MainActivity.SERVER_ADDR`
if it's necessary. 

Default `SERVER_ADDR` consist of default Genymotion ip address (`192.168.56.1`) and default server port (`5000`). For Android Emulator ip will probably be `10.0.2.2`. Read more: [Network Address Space](http://developer.android.com/tools/devices/emulator.html#networkaddresses)
