mod Location {
    use Json.JsonError.JsonError
    use Json.FromJson.{fromJson, fromJsonAt}
    use Json.Path.{Path, !!}
    use Json.{getAtKey, JsonElement, JsonError}
    
    pub enum Loc(String, String, Float64, Float64)

    instance ToString[Loc] {

        pub def toString(loc: Loc): String = 
            let Loc.Loc(city, country, lat, lon) = loc;
            let latDirection = if (lat >= 0.0) "N" else "S";
            let lonDirection = if (lon >= 0.0) "E" else "W";
            "${city} (${country}) at ${lat}°${latDirection}, ${lon}°${lonDirection}"

    }
    
    instance Json.FromJson[Loc] {
    
        pub def fromJsonAt(p: Path, json: JsonElement): Result[JsonError, Loc] = {
            forM (
                map <- fromJsonAt(p, json);
                city <- getAtKey(p, "city", map);
                country <- getAtKey(p, "country", map);
                lat <- getAtKey(p, "lat", map);
                lon <- getAtKey(p, "lon", map)
            ) yield Loc.Loc(city, country, lat, lon)
        }
    
    }

    pub eff Location {
        def getLocation(): Result[WeatherError, Loc]
    }

    ///
    /// Handles Location effect by sending http request to ip-api.com
    ///
    pub def locationWithHttp(f: Unit -> a \ ef): a \ ef - Location + HttpWithResult + IO = {
        run {
            f()
        } with handler Location {
            def getLocation(resume) = 
                let url = "http://ip-api.com/json";
                let loc = Util.getAndParse(url, Map.empty()) |> Result.flatMap ( resJson ->
                    fromJson(resJson) |> Result.mapErr( err -> 
                        WeatherError.JsonError(err)
                    )
                );
                resume(loc)
        }
    }
}
