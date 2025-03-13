mod Location {
    
    pub enum Loc(String, String, Float64, Float64)

    instance ToString[Loc] {

        pub def toString(loc: Loc): String = 
            let Loc.Loc(city, country, lat, lon) = loc;
            let latDirection = if (lat >= 0.0) "N" else "S";
            let lonDirection = if (lon >= 0.0) "E" else "W";
            "${city} (${country}) at ${lat}°${latDirection}, ${lon}°${lonDirection}"

    }

    pub eff Location {
        def getLocation(): Option[Loc]
    }

    pub def locationWithHttp(f: Unit -> a \ef ): a \ {ef - Location + HttpWithResult} = {
        run {
            f()
        } with handler Location {
            def getLocation(resume) = 
                let url = "http://ip-api.com/json";
                let locOpt = forM (
                    resJson <- requestAndParse(url, Map.empty());
                    lat <- getFloat64FromJObject("lat", resJson);
                    lon <- getFloat64FromJObject("lon", resJson);
                    city <- getStringFromJObject("city", resJson);
                    country <- getStringFromJObject("country", resJson)
                ) yield Loc.Loc(city, country, lat, lon);
                resume(locOpt)
        }
    }
}
