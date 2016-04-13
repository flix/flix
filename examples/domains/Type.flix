/**
 * An implementation of the ``type'' abstract domain.
 *
 * See Cousot & Cousot '92 for details.
 */
namespace Type {

    /**
     * The elements of the type lattice.
     */
    enum Type {
                   case Err,

        case Bool, case Int, case Real,

                   case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: Type, e2: Type): Bool = match (e1, e2) with {
        case (Type.Bot, _)              => true
        case (Type.Bool, Type.Bool)     => true
        case (Type.Int, Type.Int)       => true
        case (Type.Real, Type.Real)     => true
        case (_, Type.Err)              => true
        case _                          => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: Type, e2: Type): Type = match (e1, e2) with {
        case (Type.Bot, _)              => e2
        case (_, Type.Bot)              => e1
        case (Type.Bool, Type.Bool)     => Type.Bool
        case (Type.Int, Type.Int)       => Type.Int
        case (Type.Real, Type.Real)     => Type.Real
        case _                          => Type.Err
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: Type, e2: Type): Type = match (e1, e2) with {
        case (Type.Err, _)              => e2
        case (_, Type.Err)              => e1
        case (Type.Bool, Type.Bool)     => Type.Bool
        case (Type.Int, Type.Int)       => Type.Int
        case (Type.Real, Type.Real)     => Type.Real
        case _                          => Type.Bot
    }

    /**
      * Returns the type of the sum of two operands with the given types `e1` and `e2`.
      */
    @strict @monotone @commutative @associative
    def sum(e1: Type, e2: Type): Type = match (e1, e2) with {
        case (Type.Bot, _)              => Type.Bot
        case (_, Type.Bot)              => Type.Bot
        case (Type.Bool, Type.Bool)     => Type.Err
        case (Type.Bool, Type.Int)      => Type.Err
        case (Type.Bool, Type.Real)     => Type.Err
        case (Type.Int, Type.Bool)      => Type.Err
        case (Type.Int, Type.Int)       => Type.Int
        case (Type.Int, Type.Real)      => Type.Real
        case (Type.Real, Type.Bool)     => Type.Err
        case (Type.Real, Type.Int)      => Type.Real
        case (Type.Real, Type.Real)     => Type.Real
        case _                          => Type.Err
    }

    /**
     * Returns the type of the less-than-equal comparison of
     * two operands with the given types `e1` and `e2`.
     */
    @strict @monotone
    def lessEqual(e1: Type, e2: Type): Type = match (e1, e2) with {
        case (Type.Bot, _)              => Type.Bot
        case (_, Type.Bot)              => Type.Bot
        case (Type.Bool, Type.Bool)     => Type.Err
        case (Type.Bool, Type.Int)      => Type.Err
        case (Type.Bool, Type.Real)     => Type.Err
        case (Type.Int, Type.Bool)      => Type.Err
        case (Type.Int, Type.Int)       => Type.Bool
        case (Type.Int, Type.Real)      => Type.Bool
        case (Type.Real, Type.Bool)     => Type.Err
        case (Type.Real, Type.Int)      => Type.Bool
        case (Type.Real, Type.Real)     => Type.Bool
        case _                          => Type.Err
    }

    let Type<> = (Type.Bot, Type.Err, leq, lub, glb)

}
