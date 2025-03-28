mod Display {
    use Location.Loc
    use Weather.WeatherInfo

    pub eff Display {
        def display(loc: Loc, weather: WeatherInfo): Unit
    }

    ///
    /// Handles display effect by printing to stdout
    ///
    pub def displayWithIO(f: Unit -> a \ ef): a \ ef - Display + IO =
        run {
            f()
        } with handler Display {
            def display(loc, weather, resume) = 
                println("Weather for ${loc}:\n\t ${weather}") |> resume
        }

}
