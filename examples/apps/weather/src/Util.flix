mod Util {
    use Json.JsonElement
    use Json.Parse

    ///
    /// Sends a get http request and parse the response body to a JsonElement
    ///
    pub def getAndParse(url: String, header: Map[String, List[String]]): Result[WeatherError, JsonElement] \ HttpWithResult = 
        match HttpWithResult.get(url, header) {
            case Result.Ok(response)  => response |> Http.Response.body |> Parse.parse |> Option.toOk(WeatherError.ParseError)
            case Result.Err(err) => Err(WeatherError.RequestError(err))
        }
}
