mod Location {
    use Error.WeatherError
    use Json.getAtKey
    use Json.JsonError.JsonError
    use Json.FromJson.fromJsonAt
    use Json.{JsonElement, JsonError}
    use Json.Path.{Path, !!}
    
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

    pub def locationWithHttp(f: Unit -> a \ ef ): a \ {ef - Location + HttpWithResult + IO} = {
        run {
            f()
        } with handler Location {
            def getLocation(resume) = 
                let url = "http://ip-api.com/json";
                let loc = getAndParse(url, Map.empty()) |> Result.flatMap ( resJson ->
                    fromJsonAt(Path.Root, resJson) |> Result.mapErr( err -> 
                        WeatherError.JsonError(err)
                    )
                );
                resume(loc)
        }
    }
}
