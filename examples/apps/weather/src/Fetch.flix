mod Fetch {
    use Net.Http
    use Net.HttpResponse
    use Util.Json

    ///
    /// Sends a get http request and parses the response body to a Json value
    ///
    pub def getJson(url: String): Result[WeatherError, Json] \ Http =
        match Http.get(url) {
            case Result.Ok(response) => response |> HttpResponse.body |> Json.parse |> Result.mapErr(err -> WeatherError.JsonError(err))
            case Result.Err(err)     => Err(WeatherError.RequestError(err))
        }
}
