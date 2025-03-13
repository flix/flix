mod Weather {
    use Location.Loc
    use Json.{JsonElement, Parse}

    pub enum WeatherInfo(String, Float64)

    instance ToString[WeatherInfo] {

        pub def toString(weatherInfo: WeatherInfo): String = 
            let WeatherInfo.WeatherInfo(condition, temp) = weatherInfo;
            "${condition}, ${temp}°C"

    }

    def loadJson(f: String): Option[JsonElement] \ FileReadWithResult =
        forM (
            descriptions <- FileReadWithResult.read(f) |> Result.toOption;
            json <- Parse.parse(descriptions)
        ) yield json

    def parseWMOCode(n: Int32): Option[String] \ FileReadWithResult =
        forM (
            descriptions <- loadJson("data/description.json");
            description <- getFromJObject(ToString.toString(n), descriptions);
            dayDescription <- getFromJObject("day", description);
            content <- getStringFromJObject("description", dayDescription)
        ) yield content

    pub eff Weather {
        def getWeather(loc: Loc): Option[WeatherInfo]
    }
    
    pub def weatherWithHttpAndFile(f: Unit -> a \ ef ): a \ {ef - Weather + HttpWithResult + FileReadWithResult} = 
        run {
            f()
        } with handler Weather {
        def getWeather(loc, resume) = 
            let Loc.Loc(_, _, lat, lon) = loc;
            let url = "https://api.open-meteo.com/v1/forecast?latitude=${lat}&longitude=${lon}&current=temperature_2m,weather_code";
            let weather = forM (
                resJson <- requestAndParse(url, Map.empty());
                current <- getFromJObject("current", resJson);
                wmoCodeFloat <- getFloat64FromJObject("weather_code", current);
                wmoCode <- Float64.tryToInt32(wmoCodeFloat);
                weather <- parseWMOCode(wmoCode);
                temp <- getFloat64FromJObject("temperature_2m", current)
            ) yield WeatherInfo.WeatherInfo(weather, temp);
            resume(weather)
        }
}