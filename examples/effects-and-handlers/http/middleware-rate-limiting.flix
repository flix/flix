use Net.Http
use Net.Response

/// Uses `withSlidingWindow` to enforce a rate limit on HTTP requests.
/// At most `maxRequests` are allowed within any rolling window of `windowMs`
/// milliseconds. When the limit is reached, the handler sleeps until a slot
/// opens up.
def main(): Unit \ IO =
    // Sliding window: at most 2 requests per 1000ms window.
    run {
        let urls = List#{"/a", "/b", "/c", "/d"};
        List.forEach(url -> {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }, urls)
    } with Http.withSlidingWindow(2, 1000i64)
      with Http.withBaseUrl("https://flix.dev")
      with Http.runWithIO
