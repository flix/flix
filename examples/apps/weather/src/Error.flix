use Util.Json.JsonError

///
/// All sorts of errors we may encounter during the request
///
pub enum WeatherError {
    case RequestError(IoError),
    case JsonError(JsonError),
    case ParseWMOCodeError(String)
}

instance ToString[WeatherError] {

    pub def toString(we: WeatherError): String = match we {
        case WeatherError.RequestError(err)            => ToString.toString(err)
        case WeatherError.JsonError(err)               => ToString.toString(err)
        case WeatherError.ParseWMOCodeError(errString) => errString
    }

}

instance Formattable[WeatherError] {

    pub def format(we: WeatherError): RichString = match we {
        case WeatherError.RequestError(err)            => Formattable.format(err)
        case WeatherError.JsonError(err)               => Formattable.format(err)
        case WeatherError.ParseWMOCodeError(errString) => Formattable.format(errString)
    }

}
