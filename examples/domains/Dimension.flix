/**
 * An implementation of the dimension abstract domain.
 *
 * See Cousot & Cousot '92 for details.
 */
namespace Dimension {

    /**
     * The elements of the dimension lattice.
     */
    enum Dimension {
        case Length, case Surface, case Volume, case Time, case Velocity, case Acceleration, case Mass, case Force, case NoDim
    }

    /**
     * Concrete measures.
     */
    enum Measure {
        // Length
        case Foot, case Inch, case LightYear, case Meter, case Mile, case Parsec, case Yard,

        // Surface
        case Acre, case Hectare, case SquareMeter,

        // Volume
        case CubicMeter, case Gallon, case Litre, case Pint,

        // Time
        case Day, case Hour, case Minute, case Second, case Week, case Year
    }

    /**
     * The abstract function that maps a (concrete) measure to an (abstract) dimension.
     */
    fn alpha(m: Measure): Dimension = match m with {
        // Length
        case Measure.Foot           => Dimension.Length
        case Measure.Inch           => Dimension.Length
        case Measure.LightYear      => Dimension.Length
        case Measure.Meter          => Dimension.Length
        case Measure.Parsec         => Dimension.Length
        case Measure.Yard           => Dimension.Length

        // Surface
        case Measure.Acre           => Dimension.Surface
        case Measure.Hectare        => Dimension.Surface
        case Measure.SquareMeter    => Dimension.Surface

        // Time
        case Measure.Day            => Dimension.Time
        case Measure.Hour           => Dimension.Time
        case Measure.Minute         => Dimension.Time
        case Measure.Second         => Dimension.Time
        case Measure.Week           => Dimension.Time
        case Measure.Year           => Dimension.Time
    }


    fn sum(d1: Dimension, d2: Dimension): Dimension = match (d1, d2) with {
        case (Dimension.Length, Dimension.Length)       => Dimension.Length
        case (Dimension.Surface, Dimension.Surface)     => Dimension.Surface
        case (Dimension.Time, Dimension.Time)           => Dimension.Time
    }

    // length, su$ace, volume, time, speed, acceleration, mass, force,. . . , nodimension.

//    length rlength = length
//    length xlength = surface
//    length / length = nodimension
//    length / time = speed
//    speed / time = acceleration
//
//a( meter) = length a(pound) = mass
//cr(mile) = length cu(ton) = mass
//a(acre) = surface ff (Newton) = force
//a(second) = time (Y (nounit) = nodimension
//cY(minute) = time . . .
//cx(hour) = time
//(Y ( kilogram) = mass
//a(E, opE,) = &&&E,)
//4(E)) = T4 6)

}

// TODO: rename "lift: in other lattices to alpha?