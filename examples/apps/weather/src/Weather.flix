use Location.Loc
use Weather.WeatherInfo

pub eff Weather {
    def getWeather(loc: Loc): Result[WeatherError, WeatherInfo]
}

mod Weather {
    use Location.Loc
    use Net.Http
    use Util.Json
    use Util.Json.FromJson
    use Util.Json.FromJson.decodeAtPath
    use Util.Json.JsonError
    use Util.Json.JsonPath
    use Util.Json.JsonPath.JsonStep.Key
    use WeatherInfoPrimitive.WeatherInfoPrimitive
    use Fs.ReadFile

    pub enum WeatherInfoPrimitive(Int32, Float64)

    pub enum WeatherInfo(String, Float64)

    instance FromJson[WeatherInfoPrimitive] {

        pub def fromJson(json: Json): Result[JsonError, WeatherInfoPrimitive] =
            forM (
                wmoCodeNumber <- decodeAtPath(JsonPath.fromList(Key("current") :: Key("weather_code") :: Nil), json);
                temp <- decodeAtPath(JsonPath.fromList(Key("current") :: Key("temperature_2m") :: Nil), json)
            ) yield WeatherInfoPrimitive.WeatherInfoPrimitive(wmoCodeNumber, temp)

    }


    instance ToString[WeatherInfo] {

        pub def toString(weatherInfo: WeatherInfo): String =
            let WeatherInfo.WeatherInfo(condition, temp) = weatherInfo;
            "${condition}, ${temp}°C"

    }

    ///
    /// Reads a json file and parses it to a Json value.
    ///
    def loadJson(f: String): Result[WeatherError, Json] \ ReadFile =
        match ReadFile.read(f) {
            case Result.Ok(descriptions) => Json.parse(descriptions) |> Result.mapErr(err -> WeatherError.JsonError(err))
            case Result.Err(_)           => Err(WeatherError.ParseWMOCodeError("File ${f} doesn't exist."))
        }

    ///
    /// Translates a WMOCode to corresponding weather condition.
    /// We rely on data/description.json to translate.
    ///
    /// The keys of data/description.json are numeric strings, so the path is built
    /// from `Key` steps (a JSON pointer like `/45/day` would read `45` as an index).
    ///
    def parseWMOCode(n: Int32): Result[WeatherError, String] \ ReadFile =
        loadJson("data/description.json") |> Result.flatMap(descriptions ->
            decodeAtPath(JsonPath.fromList(Key("${n}") :: Key("day") :: Key("description") :: Nil), descriptions)
            |> Result.mapErr(err -> WeatherError.JsonError(err))
        )


    ///
    /// Handles Weather effect by getting the weather for the given location from open-meteo and parse it.
    ///
    pub def weatherWithHttpAndFile(f: Unit -> a \ ef): a \ ef - Weather + Http + ReadFile =
        run {
            f()
        } with handler Weather {
        def getWeather(loc, resume) =
            let Loc.Loc(_, _, lat, lon) = loc;
            let url = "https://api.open-meteo.com/v1/forecast?latitude=${lat}&longitude=${lon}&current=temperature_2m,weather_code";
            let weather = Fetch.getJson(url) |> Result.flatMap( resJson ->
                match FromJson.fromJson(resJson) {
                    case Result.Ok(weatherInfoPrimitive)  =>
                        let WeatherInfoPrimitive(wmoCode, temp) = weatherInfoPrimitive;
                        parseWMOCode(wmoCode) |> Result.flatMap( weatherCondition ->
                            Ok(WeatherInfo.WeatherInfo(weatherCondition, temp))
                        )
                    case Result.Err(err) => Err(WeatherError.JsonError(err))
                }
            );
            resume(weather)
        }
}
