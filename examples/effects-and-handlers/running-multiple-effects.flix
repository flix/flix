def main(): Unit \ {Net, IO} =
    run {
        let url = "http://example.com/";
        Logger.info("Downloading URL: '${url}'");
        match HttpWithResult.get(url, Map.empty()) {
            case Result.Ok(response) =>
                let file = "data.txt";
                Logger.info("Saving response to file: '${file}'");
                let body = Http.Response.body(response);
                match FileWriteWithResult.write(str = body, file) {
                    case Result.Ok(_) =>
                        Logger.info("Response saved to file: '${file}'")
                    case Result.Err(err) =>
                        Logger.fatal("Unable to write file: '${err}'")
                }
            case Result.Err(err) =>
                Logger.fatal("Unable to download URL: '${err}'")
        }
    } with FileWriteWithResult.runWithIO
      with HttpWithResult.runWithIO
      with Logger.runWithIO
