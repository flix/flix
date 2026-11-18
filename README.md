<p align="center" >
    <img src="https://raw.githubusercontent.com/flix/flix/master/docs/logo.png" height="91px" 
    alt="The Flix Programming Language" 
    title="The Flix Programming Language">
</p>

**Flix** is a powerful effect-oriented programming language.

We refer you to the [official Flix website (flix.dev)](https://flix.dev/) for more information about Flix. 

[![Build and Tests](https://img.shields.io/github/actions/workflow/status/flix/flix/compiler-build-and-test.yaml?label=build%20and%20tests&branch=master)](https://github.com/flix/flix/actions/workflows/compiler-build-and-test.yaml)
[![Community Build](https://img.shields.io/github/actions/workflow/status/flix/flix/community-build.yaml?label=community%20build&branch=master)](https://github.com/flix/flix/actions/workflows/community-build.yaml)
[![Release](https://img.shields.io/github/v/release/flix/flix?label=release)](https://github.com/flix/flix/releases)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue)](https://github.com/flix/flix/blob/master/LICENSE.md)
[![Zulip](https://img.shields.io/badge/zulip-join_chat-brightgreen.svg)](https://flix.zulipchat.com/)

## Example

```flix
use Net.Http
use Net.Retry
use Net.HttpResponse
use Time.Clock
use Time.Duration.{milliseconds, seconds}

// An example showing use of multiple effects and effect handlers.
def main(): Unit \ { Clock, Http, Logger, IO } =
    let defaultHeaders = Map#{
        "Accept"        => List#{"application/json"},
        "Authorization" => List#{"Bearer tok123"}
    };
    run {
        let urls = List#{"/api/users", "/api/posts"};
        foreach (url <- urls) {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${HttpResponse.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        };
        match Http.get("https://notfound.flix.dev/") {
            case Ok(resp) => println("notfound -> ${HttpResponse.status(resp)}")
            case Err(err) => println("notfound -> ${err}")
        }
    } with Http.withBaseUrl("https://flix.dev")
      with Http.withDefaultHeaders(defaultHeaders)
      with Http.withRetry(Retry.linear(maxRetries = 2, delay = milliseconds(100)))
      with Http.withCircuitBreaker(failureThreshold = 3, cooldown = seconds(5))
      with Http.withSlidingWindow(maxRequests = 2, window = seconds(1))
      with Http.withLogging
```

## Building

See [docs/BUILD.md](docs/BUILD.md).

## License

Flix is available under the Apache 2.0 license.

## Sponsors

We kindly thank [EJ Technologies](https://www.ej-technologies.com/) for providing us with 
[JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html)
and [JetBrains](https://www.jetbrains.com/) for providing us with 
[IntelliJ IDEA](https://www.jetbrains.com/idea/).
