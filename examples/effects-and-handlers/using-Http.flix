def main(): Unit \ {Net, IO} =
    run {
        match HttpWithResult.get("http://example.com/", Map.empty()) {
            case Result.Ok(response) =>
                let body = Http.Response.body(response);
                println(body)
            case Result.Err(e) => println(e)
        }
    } with HttpWithResult.runWithIO
