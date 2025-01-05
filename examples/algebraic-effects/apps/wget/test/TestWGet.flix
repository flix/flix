mod TestWGet {

    @Test
    def downloadWritesTheOutputReceivedFromHttp(): Bool = {
        run {
            let opts = { targetFile = "myfile" };
            let url = "flix.dev";
            WGet.download(opts, url);
            false
        } with Http {
            def request(_, url, _, _, k) = k(Http.Response.Response({ status = 200, headers = Map.empty(), body = "this is the content" }))
        } with FileWrite {
            def write(data, _, _) =
                Assert.eq("this is the content", data#str)
            def writeLines(data, f, k) = ???
            def writeBytes(data, f, k) = ???
            def append(data, f, k) = ???
            def appendLines(data, f, k) = ???
            def appendBytes(data, f, k) = ???
            def truncate(f, k) = ???
            def mkDir(d, k) = ???
            def mkDirs(d, k) = ???
        }
    }

    @Test
    def downloadShouldWriteToGivenFile(): Bool = {
        run {
            let opts = { targetFile = "myfile" };
            let url = "flix.dev";
            WGet.download(opts, url);
            false
        } with Http {
            def request(_, url, _, _, k) = k(Http.Response.Response({ status = 200, headers = Map.empty(), body = "this is the content" }))
        } with FileWrite {
            def write(_, f, _) =
                Assert.eq("myfile", f)
            def writeLines(data, f, k) = ???
            def writeBytes(data, f, k) = ???
            def append(data, f, k) = ???
            def appendLines(data, f, k) = ???
            def appendBytes(data, f, k) = ???
            def truncate(f, k) = ???
            def mkDir(d, k) = ???
            def mkDirs(d, k) = ???
        }
    }

}
