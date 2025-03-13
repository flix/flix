use Error.WeatherError
use Json.JsonElement
use Json.Parse

def getAndParse(url: String, header: Map[String, List[String]]): Result[WeatherError, JsonElement] \ HttpWithResult = 
    match HttpWithResult.get(url, header) {
        case Result.Ok(response)  => response |> Http.Response.body |> Parse.parse |> Option.toOk(WeatherError.ParseError)
        case Result.Err(err) => Err(WeatherError.RequestError(err))
    }
