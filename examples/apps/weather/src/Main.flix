def main(): Unit \ {Net, IO} = 
    run {
        foreach (
            loc <- Location.Location.getLocation();
            weather <- Weather.Weather.getWeather(loc)
        ) Display.Display.display(loc, weather)
    } with Weather.weatherWithHttpAndFile
    with Location.locationWithHttp
    with Display.displayWithIO
    with HttpWithResult.runWithIO
    with Logger.runWithIO
    with FileReadWithResult.runWithIO