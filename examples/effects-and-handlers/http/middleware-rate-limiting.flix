use Net.Http
use Net.Response

/// Demonstrates three rate-limiting strategies provided by `Net.Http`:
/// - `withMinInterval`:   fixed minimum delay between consecutive requests.
/// - `withTokenBucket`:   burst of requests up front, then a steady rate.
/// - `withSlidingWindow`: at most N requests in any rolling window.
def main(): Unit \ { Clock, Http, IO } =
    // Min interval: at least 100ms between consecutive requests.
    run {
        println("--- Min interval ---");
        let urls = List#{"/a", "/b", "/c", "/d"};
        foreach (url <- urls) {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }
    } with Http.withMinInterval(100i64)
      with Http.withBaseUrl("https://flix.dev");

    // Token bucket: burst of 2, then 1 request per 100ms.
    run {
        println("--- Token bucket ---");
        let urls = List#{"/a", "/b", "/c", "/d"};
        foreach (url <- urls) {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }
    } with Http.withTokenBucket(2, 100i64)
      with Http.withBaseUrl("https://flix.dev");

    // Sliding window: at most 2 requests per 1000ms window.
    run {
        println("--- Sliding window ---");
        let urls = List#{"/a", "/b", "/c", "/d"};
        foreach (url <- urls) {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }
    } with Http.withSlidingWindow(2, 1000i64)
      with Http.withBaseUrl("https://flix.dev")
