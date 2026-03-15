use Net.Http
use Net.HttpResponse

def main(): Unit \ { Http, Logger, IO } =
    run {
        let url = "http://example.com/";
        Logger.info("Downloading URL: '${url}'");
        match Http.get(url) {
            case Ok(resp) =>
                let file = "data.txt";
                Logger.info("Saving response to file: '${file}'");
                let body = HttpResponse.body(resp);
                match Fs.WriteFile.write(str = body, file) {
                    case Ok(_) =>
                        Logger.info("Response saved to file: '${file}'")
                    case Err(err) =>
                        Logger.fatal("Unable to write file: '${err}'")
                }
            case Err(err) =>
                Logger.fatal("Unable to download URL: '${err}'")
        }
    } with Fs.WriteFile.runWithIO
