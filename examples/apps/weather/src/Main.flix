use Fs.FileRead
use Fs.ReadFile
use Net.Http

///
/// Prints the weather condition an temperature based on the user's location.
///
def main(): Unit \ IO =
    run {
        match Location.getLocation() {
            case Err(err) => Logger.fatal(err)
            case Ok(loc) => match Weather.getWeather(loc) {
                case Err(err) => Logger.fatal(err)
                case Ok(weather) => Display.display(loc, weather)
            }
        }
    } with Weather.weatherWithHttpAndFile
    with Location.locationWithHttp
    with Display.displayWithIO
    with Http.runWithIO
    with Logger.runWithIO
    with ReadFile.runWithFileRead
    with FileRead.runWithIO
