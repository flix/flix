/**
 * An implementation of the dimension abstract domain.
 *
 * See Cousot & Cousot '92 for details.
 */
namespace Dimension {

    // TODO: Throw away?

    /**
     * The elements of the dimension lattice.
     */
    enum Dimension {
        case Length, case Surface, case Volume, case Time, case Velocity, case Acceleration, case Mass, case Force, case Unitless
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
        case Day, case Hour, case Minute, case Second, case Week, case Year,

        // Velocity
       case KilometersPerHour, case Knots, case MetersPerSecond, case MilesPerHour,

       // Acceleration
       case MetersPerSecond2,

       // Mass
       case Gram, case Kilogram, case Ounce, case Pound, case Ton,

       // Force
       case Newton
    }

    /**
     * The abstract function that maps a (concrete) measure to an (abstract) dimension.
     */
    fn alpha(m: Measure): Dimension = match m with {
        // Length
        case Measure.Foot               => Dimension.Length
        case Measure.Inch               => Dimension.Length
        case Measure.LightYear          => Dimension.Length
        case Measure.Meter              => Dimension.Length
        case Measure.Parsec             => Dimension.Length
        case Measure.Yard               => Dimension.Length

        // Surface
        case Measure.Acre               => Dimension.Surface
        case Measure.Hectare            => Dimension.Surface
        case Measure.SquareMeter        => Dimension.Surface

        // Time
        case Measure.Day                => Dimension.Time
        case Measure.Hour               => Dimension.Time
        case Measure.Minute             => Dimension.Time
        case Measure.Second             => Dimension.Time
        case Measure.Week               => Dimension.Time
        case Measure.Year               => Dimension.Time

        // Velocity
        case Measure.KilometersPerHour  => Dimension.Velocity
        case Measure.Knots              => Dimension.Velocity
        case Measure.MetersPerSecond    => Dimension.Velocity
        case Measure.MilesPerHour       => Dimension.Velocity

        // Acceleration
        case Measure.MetersPerSecond2   => Dimension.Acceleration

        // Mass
        case Measure.Gram               => Dimension.Mass
        case Measure.Kilogram           => Dimension.Mass
        case Measure.Ounce              => Dimension.Mass
        case Measure.Pound              => Dimension.Mass
        case Measure.Ton                => Dimension.Mass

        // Force
        case Measure.Newton             => Dimension.Force
    }

    /**
     * Returns the dimension of the product of the given dimensions `e1` and `e2`.
     */
    @strict @monotone @commutative
    fn times(d1: Dimension, d2: Dimension): Dimension = match (d1, d2) with {
        case (Dimension.Unitless, _)                    => d2
        case (_, Dimension.Unitless)                    => d1

        case (Dimension.Length, Dimension.Length)       => Dimension.Surface

        case (Dimension.Length, Dimension.Surface)      => Dimension.Volume
        case (Dimension.Surface, Dimension.Length)      => Dimension.Volume

        case (Dimension.Time, Dimension.Velocity)       => Dimension.Length
        case (Dimension.Velocity, Dimension.Time)       => Dimension.Length

        case (Dimension.Time, Dimension.Acceleration)   => Dimension.Velocity
        case (Dimension.Acceleration, Dimension.Time)   => Dimension.Velocity

        case (Dimension.Mass, Dimension.Acceleration)   => Dimension.Force
        case (Dimension.Acceleration, Dimension.Mass)   => Dimension.Force

        case _ => ???: Dimension // TODO
    }

    /**
     * Returns the dimension of the division of the given dimensions `e1` and `e2`.
     */
    @strict @monotone
    fn divide(d1: Dimension, d2: Dimension): Dimension = match (d1, d2) with {
        case (Dimension.Unitless, _)                    => d2
        case (_, Dimension.Unitless)                    => d1

        case (Dimension.Surface, Dimension.Length)      => Dimension.Length

        case (Dimension.Volume, Dimension.Length)       => Dimension.Surface
        case (Dimension.Volume, Dimension.Surface)      => Dimension.Length

        case (Dimension.Length, Dimension.Time)         => Dimension.Velocity

        case (Dimension.Velocity, Dimension.Time)       => Dimension.Acceleration

        case (Dimension.Force, Dimension.Mass)          => Dimension.Acceleration

        case _ =>
            if (d1 == d2)
                Dimension.Unitless
            else
                ???: Dimension // TODO
    }

}
