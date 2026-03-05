use Net.Http
use Net.Retry
use Net.Response

/// Demonstrates three retry strategies provided by `Net.Retry`:
/// - `linear`:             fixed delay between retries.
/// - `exponential`:        doubling delay between retries.
/// - `retryTransportOnly`: retries only on connection failures, not HTTP errors.
def main(): Unit \ IO =
    // Linear: retry up to 3 times with 500ms delay.
    run {
        println("--- Linear retry ---");
        match Http.get("https://flix.dev/") {
            case Ok(resp) => println("Status: ${Response.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withRetry(Retry.linear(3, 500i64))
      with Http.runWithIO;

    // Exponential: retry up to 3 times with 200ms base delay.
    run {
        println("--- Exponential retry ---");
        match Http.get("https://flix.dev/") {
            case Ok(resp) => println("Status: ${Response.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withRetry(Retry.exponential(3, 200i64))
      with Http.runWithIO;

    // Transport-only: only retry on connection failures, not HTTP errors.
    run {
        println("--- Transport-only retry ---");
        match Http.get("https://flix.dev/") {
            case Ok(resp) => println("Status: ${Response.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withRetry(Retry.retryTransportOnly(2, 1000i64))
      with Http.runWithIO
