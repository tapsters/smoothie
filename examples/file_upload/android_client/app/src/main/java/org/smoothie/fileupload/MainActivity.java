package org.smoothie.fileupload;

import android.content.Intent;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.provider.MediaStore;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpInputStream;
import com.koushikdutta.async.ByteBufferList;
import com.koushikdutta.async.DataEmitter;
import com.koushikdutta.async.callback.CompletedCallback;
import com.koushikdutta.async.callback.DataCallback;
import com.koushikdutta.async.http.AsyncHttpClient;
import com.koushikdutta.async.http.WebSocket;

import org.apache.commons.io.IOUtils;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MainActivity extends AppCompatActivity {

    private static final String TAG_WS = "WEBSOCKET";
    private static final int RESULT_LOAD_IMAGE = 1;
    private static final String SERVER_ADDR = "192.168.56.1:5000";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Button chooseImageButton = (Button) findViewById(R.id.chooseImageButton);
        chooseImageButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent data = new Intent(Intent.ACTION_PICK,
                        android.provider.MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
                startActivityForResult(data, RESULT_LOAD_IMAGE);
            }
        });
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        if (requestCode == RESULT_LOAD_IMAGE && resultCode == RESULT_OK && null != data) {
            Uri selectedImage = data.getData();
            String[] filePathColumn = { MediaStore.Images.Media.DATA };

            Cursor cursor = getContentResolver().query(selectedImage,
                    filePathColumn, null, null, null);
            cursor.moveToFirst();

            int columnIndex = cursor.getColumnIndex(filePathColumn[0]);
            String picturePath = cursor.getString(columnIndex);
            cursor.close();

            FileInputStream fileStream = null;

            try {
                fileStream = new FileInputStream(picturePath);
                final byte[] fileData = IOUtils.toByteArray(fileStream);
                List<byte[]> chunks = divideArray(fileData, 4000);

                connectToSocketAndSendImage(chunks, picturePath, fileData.length);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                IOUtils.closeQuietly(fileStream);
            }
        }
    }

    public List<byte[]> divideArray(byte[] source, int chunksize) {
        List<byte[]> result = new ArrayList<byte[]>();
        int start = 0;
        while (start < source.length) {
            int end = Math.min(source.length, start + chunksize);
            result.add(Arrays.copyOfRange(source, start, end));
            start += chunksize;
        }

        return result;
    }

    private void connectToSocketAndSendImage(final List<byte[]> chunks, final String name, final int length) {
        String ext = name.substring(name.lastIndexOf(".") + 1);
        String url = "ws://" + SERVER_ADDR + "/upload/?size=" + String.valueOf(length) + "&ext=" + ext;
        AsyncHttpClient.getDefaultInstance().websocket(url, null, new AsyncHttpClient.WebSocketConnectCallback() {

            private WebSocket webSocketConnected;

            @Override
            public void onCompleted(Exception ex, WebSocket webSocket) {
                if (webSocket != null) {
                    webSocketConnected = webSocket;

                    webSocketConnected.setDataCallback(new DataCallback() {
                        @Override
                        public void onDataAvailable(DataEmitter emitter, ByteBufferList bb) {
                            Log.println(Log.DEBUG, TAG_WS, "Data Received");
                            try {
                                OtpInputStream stream = new OtpInputStream(bb.getAllByteArray());
                                OtpErlangObject object = stream.read_any();
                                if (object instanceof OtpErlangList) {
                                    String status = new String(((OtpErlangBinary)((OtpErlangTuple)((OtpErlangList) object).elementAt(0)).elementAt(1)).binaryValue());
                                    if (status.equals("ready" )) {
                                        for(byte[] chunk:chunks) {
                                            webSocketConnected.send(chunk);
                                        }
                                        webSocketConnected.send(("done").getBytes());
                                    } else if (status.equals("complete")) {
                                        String url = new String(((OtpErlangBinary)((OtpErlangTuple)((OtpErlangList) object).elementAt(1)).elementAt(1)).binaryValue());
                                        final String fullUrl = "http://" + SERVER_ADDR + url;
                                        final Bitmap uploadedImageBitmap = getBitmapFromURL(fullUrl);
                                        runOnUiThread(new Runnable() {
                                            @Override
                                            public void run() {
                                                TextView uploadedImageUrlText = (TextView) findViewById(R.id.uploadedImageUrlText);
                                                uploadedImageUrlText.setText(fullUrl);

                                                ImageView uploadedImageView = (ImageView) findViewById(R.id.uploadedImageView);
                                                uploadedImageView.setImageBitmap(uploadedImageBitmap);
                                            }
                                        });
                                        Log.println(Log.DEBUG, TAG_WS, url);
                                    }
                                }
                                stream.close();
                            } catch (OtpErlangDecodeException e) {
                                e.printStackTrace();
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        }
                    });

                    webSocketConnected.setClosedCallback(new CompletedCallback() {
                        @Override
                        public void onCompleted(Exception ex) {
                            Log.println(Log.DEBUG, TAG_WS, "Closed");
                        }
                    });

                    webSocketConnected.setEndCallback(new CompletedCallback() {
                        @Override
                        public void onCompleted(Exception ex) {
                            Log.println(Log.DEBUG, TAG_WS, "Ended");
                        }
                    });
                }
            }
        });
    }

    public static Bitmap getBitmapFromURL(String src) {
        try {
            URL url = new URL(src);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setDoInput(true);
            connection.connect();
            InputStream input = connection.getInputStream();
            return BitmapFactory.decodeStream(input);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();

        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }
}
