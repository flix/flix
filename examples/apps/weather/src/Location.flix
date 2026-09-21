mod Location {
    use Net.Http
    use Util.Json
    use Util.Json.FromJson
    use Util.Json.FromJson.decodeAtKey
    use Util.Json.JsonError

    pub eff Location {
        def getLocation(): Result[WeatherError, Loc]
    }

    pub enum Loc(String, String, Float64, Float64)

    instance ToString[Loc] {

        pub def toString(loc: Loc): String =
            let Loc.Loc(city, country, lat, lon) = loc;
            let latDirection = if (lat >= 0.0) "N" else "S";
            let lonDirection = if (lon >= 0.0) "E" else "W";
            "${city} (${country}) at ${lat}°${latDirection}, ${lon}°${lonDirection}"

    }

    instance FromJson[Loc] {

        pub def fromJson(json: Json): Result[JsonError, Loc] = {
            forM (
                city <- decodeAtKey("city", json);
                country <- decodeAtKey("country", json);
                lat <- decodeAtKey("lat", json);
                lon <- decodeAtKey("lon", json)
            ) yield Loc.Loc(city, country, lat, lon)
        }

    }

    ///
    /// Handles Location effect by sending http request to ip-api.com
    ///
    pub def locationWithHttp(f: Unit -> a \ ef): a \ ef - Location + Http = {
        run {
            f()
        } with handler Location {
            def getLocation(resume) =
                let url = "http://ip-api.com/json";
                let loc = Fetch.getJson(url) |> Result.flatMap ( resJson ->
                    FromJson.fromJson(resJson) |> Result.mapErr( err ->
                        WeatherError.JsonError(err)
                    )
                );
                resume(loc)
        }
    }
}
