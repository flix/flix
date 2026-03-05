use Net.Http
use Net.Response

/// Demonstrates three rate-limiting strategies provided by `Net.Http`:
/// - `withMinInterval`:   fixed minimum delay between consecutive requests.
/// - `withTokenBucket`:   burst of requests up front, then a steady rate.
/// - `withSlidingWindow`: at most N requests in any rolling window.
def main(): Unit \ {Clock, IO} =
    // Min interval: at least 100ms between consecutive requests.
    run {
        println("--- Min interval ---");
        let urls = List#{"/a", "/b", "/c", "/d"};
        List.forEach(url -> {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }, urls)
    } with Http.withMinInterval(100i64)
      with Http.withBaseUrl("https://flix.dev")
      with Http.runWithIO;

    // Token bucket: burst of 2, then 1 request per 100ms.
    run {
        println("--- Token bucket ---");
        let urls = List#{"/a", "/b", "/c", "/d"};
        List.forEach(url -> {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }, urls)
    } with Http.withTokenBucket(2, 100i64)
      with Http.withBaseUrl("https://flix.dev")
      with Http.runWithIO;

    // Sliding window: at most 2 requests per 1000ms window.
    run {
        println("--- Sliding window ---");
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
