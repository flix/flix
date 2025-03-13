mod Error{
    use Error.WeatherError
    use Json.JsonError

    pub enum WeatherError{
        case RequestError(IoError),
        case JsonError(JsonError),
        case ParseError,
        case ParseWMOCodeError(String)
    }
    
    instance ToString[WeatherError] {
    
        pub def toString(we: WeatherError): String = match we {
            case Error.WeatherError.RequestError(err)      => ToString.toString(err)
            case Error.WeatherError.JsonError(_err)         => "JsonError"
            case Error.WeatherError.ParseError               => "ParseError"
            case Error.WeatherError.ParseWMOCodeError(errString) => errString
        }
    
    }
    

}
