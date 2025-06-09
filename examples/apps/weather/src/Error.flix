use Json.JsonError

///
/// All sorts of errors we may encounter during the request
///
pub enum WeatherError {
    case RequestError(IoError),
    case JsonError(JsonError),
    case ParseError,
    case ParseWMOCodeError(String)
}

instance ToString[WeatherError] {

    pub def toString(we: WeatherError): String = match we {
        case WeatherError.RequestError(err)            => ToString.toString(err)
        case WeatherError.JsonError(_err)              => "JsonError"
        case WeatherError.ParseError                   => "ParseError"
        case WeatherError.ParseWMOCodeError(errString) => errString
    }

}
