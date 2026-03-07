use Net.Http
use Net.Retry
use Net.HttpResponse
use Time.Duration.milliseconds

/// Demonstrates three retry strategies provided by `Net.Retry`:
/// - `linear`:             fixed delay between retries.
/// - `exponential`:        doubling delay between retries.
/// - `retryTransportOnly`: retries only on connection failures, not HTTP errors.
def main(): Unit \ { Http, IO } =
    // Linear: retry up to 3 times with 100ms delay.
    run {
        println("--- Linear retry ---");
        match Http.get("https://notfound.flix.dev/") {
            case Ok(resp) => println("Status: ${HttpResponse.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withRetry(Retry.linear(maxRetries = 3, delay = milliseconds(100)));

    // Exponential: retry up to 3 times with 100ms base delay.
    run {
        println("--- Exponential retry ---");
        match Http.get("https://notfound.flix.dev/") {
            case Ok(resp) => println("Status: ${HttpResponse.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withRetry(Retry.exponential(maxRetries = 3, baseDelay = milliseconds(100)));

    // Transport-only: only retry on connection failures, not HTTP errors.
    run {
        println("--- Transport-only retry ---");
        match Http.get("https://notfound.flix.dev/") {
            case Ok(resp) => println("Status: ${HttpResponse.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withRetry(Retry.retryTransportOnly(maxRetries = 2, delay = milliseconds(100)))
