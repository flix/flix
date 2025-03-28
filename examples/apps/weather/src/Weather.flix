mod Weather {
    use Json.{FromJson, getAtKey, JsonElement, JsonError, Parse}
    use Json.FromJson.{fromNullableJson, fromJson,fromJsonAt} 
    use Json.Path.{!!, Path}
    use Location.Loc
    use WeatherInfo.WeatherInfo
    use WeatherError.WeatherError
    use WeatherInfoPrimitive.WeatherInfoPrimitive
    
    pub enum WeatherInfoPrimitive(Int32, Float64)
    
    pub enum WeatherInfo(String, Float64)
    
    instance FromJson[WeatherInfoPrimitive] {
    
        pub def fromJsonAt(p: Path, json: JsonElement): Result[JsonError, WeatherInfoPrimitive] = 
            forM (
                resMap <- fromJsonAt(p, json);
                currentMap <- getAtKey(p, "current", resMap);
                wmoCodeNumber <- getAtKey(p !! "current", "weather_code", currentMap);
                temp <- getAtKey(p !! "current", "temperature_2m", currentMap)
            ) yield WeatherInfoPrimitive.WeatherInfoPrimitive(wmoCodeNumber, temp)
    
    }
    

    instance ToString[WeatherInfo] {

        pub def toString(weatherInfo: WeatherInfo): String = 
            let WeatherInfo.WeatherInfo(condition, temp) = weatherInfo;
            "${condition}, ${temp}°C"

    }

    ///
    /// Reads a json file and parse it to JsonEelement.
    /// Any failure during the process will result in a None.
    ///
    def loadJson(f: String): Option[JsonElement] \ FileReadWithResult =
        forM (
            descriptions <- FileReadWithResult.read(f) |> Result.toOption;
            json <- Parse.parse(descriptions)
        ) yield json

    ///
    /// Translates a WMOCode to corresponding weather condition.
    /// We rely on data/description.json to translate.
    ///
    def parseWMOCode(n: Int32): Result[WeatherError, String] \ FileReadWithResult =
        match loadJson("data/description.json") {
            case Option.None        => Err(WeatherError.ParseWMOCodeError("File data/description.json doesn't exist."))
            case Option.Some(descriptions) => 
                let nString = ToString.toString(n);
                let res = forM (
                    descriptionsMap <- fromJsonAt(Path.Root, descriptions);
                    descriptionMap <- getAtKey(Path.Root, nString, descriptionsMap);
                    dayDescriptionMap <- getAtKey(Path.Root !! nString, "day", descriptionMap);
                    content <- getAtKey(Path.Root !! nString !! "day" , "description", dayDescriptionMap)
                ) yield content;
                res |> Result.mapErr( err ->
                    WeatherError.JsonError(err)
                )
        }

    pub eff Weather {
        def getWeather(loc: Loc): Result[WeatherError, WeatherInfo]
    }
    
    ///
    /// Handles Weather effect by getting the weather for the given location from open-meteo and parse it.
    ///
    pub def weatherWithHttpAndFile(f: Unit -> a \ ef): a \ ef - Weather + HttpWithResult + FileReadWithResult = 
        run {
            f()
        } with handler Weather {
        def getWeather(loc, resume) = 
            let Loc.Loc(_, _, lat, lon) = loc;
            let url = "https://api.open-meteo.com/v1/forecast?latitude=${lat}&longitude=${lon}&current=temperature_2m,weather_code";
            let weather = Util.getAndParse(url, Map.empty()) |> Result.flatMap( resJson ->
                match fromJson(resJson) {
                    case Result.Ok(weatherInfoPrimitive)  => 
                        let WeatherInfoPrimitive(wmo_code, temp) = weatherInfoPrimitive;
                        parseWMOCode(wmo_code) |> Result.flatMap( weatherCondition ->
                            Ok(WeatherInfo(weatherCondition, temp))
                        )
                    case Result.Err(err) => Err(WeatherError.JsonError(err))
                }
            );
            resume(weather)
        }
}
