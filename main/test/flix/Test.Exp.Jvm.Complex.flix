mod Test.Exp.Jvm.Complex {

    import java.io.IOException;
    import java.io.OutputStream;
    import java.net.InetSocketAddress;
    import java.util.concurrent.Executors;

    import com.sun.net.httpserver.HttpExchange;
    import com.sun.net.httpserver.HttpHandler;
    import com.sun.net.httpserver.HttpServer;

    pub def run(): Unit \ {Net, IO} =
        let server = HttpServer.create(new InetSocketAddress(3000), 0);
        server.createContext("/", handler());
        ()

    def handler(): HttpHandler \ IO = new HttpHandler {
        def handle(_this:HttpHandler, t:HttpExchange): Unit \ IO =
          let response = "Hello world!";
          t.sendResponseHeaders(200, Int32.toInt64(response.length()));
          let os = t.getResponseBody();
          os.write(response.getBytes());
          os.close()
    }

}
