mod WGet {

    type alias Options = { targetFile = String }

    pub def download(options: Options, url: String): Unit \ FileWrite + Http = {
        getResource(options, url = url) |> writeToDisk(options)
    }

    def getResource(_options: Options, url: { url = String }): String \ Http = {
        Http.get(url#url, Map.empty()) |> Http.Response.body
    }

    def writeToDisk(options: Options, content: String): Unit \ FileWrite = {
        FileWrite.write(str = content, options#targetFile)
    }

}
