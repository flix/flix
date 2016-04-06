/**
 * An implementation of the parity abstract domain.
 */
namespace Parity {

    /**
     * The elements of the parity lattice.
     */
    enum Parity {
                  case Top,

        case Odd,          case Even,

                  case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: Parity, e2: Parity): Bool = match (e1, e2) with {
        case (Parity.Bot, _)              => true
        case (Parity.Odd, Parity.Odd)     => true
        case (Parity.Even, Parity.Even)   => true
        case (_, Parity.Top)              => true
        case _                            => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    fn lub(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Parity.Bot, x)              => x
        case (x, Parity.Bot)              => x
        case (Parity.Odd, Parity.Odd)     => Parity.Odd
        case (Parity.Even, Parity.Even)   => Parity.Even
        case _                            => Parity.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    fn glb(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Parity.Top, x)                => x
        case (x, Parity.Top)                => x
        case (Parity.Odd, Parity.Odd)       => Parity.Odd
        case (Parity.Even, Parity.Even)     => Parity.Even
        case _                              => Parity.Bot
    }

    /**
     * Abstracts a concrete integer into the Parity domain.
     */
    def alpha(i: Int): Parity = if (i % 2 != 0) Parity.Odd else Parity.Even

    // TODO: inc/dec

    /**
     * Over-approximates integer `addition`.
     */
    @strict @monotone @commutative @associative
    fn plus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Parity.Bot)              => Parity.Bot
        case (Parity.Bot, _)              => Parity.Bot
        case (Parity.Odd, Parity.Odd)     => Parity.Even
        case (Parity.Odd, Parity.Even)    => Parity.Odd
        case (Parity.Even, Parity.Odd)    => Parity.Odd
        case (Parity.Even, Parity.Even)   => Parity.Even
        case _                            => Parity.Top
    }

    /**
     * Over-approximates integer `subtraction`.
     */
    @strict @monotone
    fn minus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Parity.Bot)              => Parity.Bot
        case (Parity.Bot, _)              => Parity.Bot
        case (Parity.Odd, Parity.Odd)     => Parity.Even
        case (Parity.Odd, Parity.Even)    => Parity.Odd
        case (Parity.Even, Parity.Odd)    => Parity.Odd
        case (Parity.Even, Parity.Even)   => Parity.Even
        case _                            => Parity.Top
    }

    /**
     * Over-approximates integer `multiplication`.
     */
    @strict @monotone @commutative @associative
    fn times(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Parity.Bot)              => Parity.Bot
        case (Parity.Bot, _)              => Parity.Bot
        case (Parity.Odd, Parity.Odd)     => Parity.Odd
        case (Parity.Odd, Parity.Even)    => Parity.Even
        case (Parity.Even, Parity.Odd)    => Parity.Even
        case (Parity.Even, Parity.Even)   => Parity.Even
        case _                            => Parity.Top
    }

    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone
    fn divide(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
       case (_, Parity.Bot)              => Parity.Bot
       case (Parity.Bot, _)              => Parity.Bot
       case _                            => Parity.Top
    }

    /**
     * Over-approximates integer `modulus`.
     */
    @strict @monotone
    fn modulo(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
       case (_, Parity.Bot)              => Parity.Bot
       case (Parity.Bot, _)              => Parity.Bot
       case (Parity.Odd, Parity.Odd)     => Parity.Top
       case (Parity.Even, Parity.Odd)    => Parity.Top
       case (Parity.Odd, Parity.Even)    => Parity.Odd
       case (Parity.Even, Parity.Even)   => Parity.Even
       case _                            => Parity.Top
    }

    /**
     * Over-approximates `bitwise negation`.
     */
    @strict @monotone
    fn negate(e: Parity): Parity = match e with {
        case Parity.Bot     =>  Parity.Bot
        case Parity.Odd     =>  Parity.Even
        case Parity.Even    =>  Parity.Odd
        case Parity.Top     =>  Parity.Top
    }

    /**
     * Over-approximates `bitwise or`.
     */
    @strict @monotone @commutative @associative
    fn or(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Parity.Bot)              => Parity.Bot
      case (Parity.Bot, _)              => Parity.Bot
      case (Parity.Odd, Parity.Odd)     => Parity.Odd
      case (Parity.Even, Parity.Odd)    => Parity.Odd
      case (Parity.Odd, Parity.Even)    => Parity.Odd
      case (Parity.Even, Parity.Even)   => Parity.Even
      case _                            => Parity.Top
    }

    /**
     * Over-approximates `bitwise and`.
     */
    @strict @monotone @commutative @associative
    fn and(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
       case (_, Parity.Bot)              => Parity.Bot
       case (Parity.Bot, _)              => Parity.Bot
       case (Parity.Odd, Parity.Odd)     => Parity.Odd
       case (Parity.Even, Parity.Odd)    => Parity.Even
       case (Parity.Odd, Parity.Even)    => Parity.Even
       case (Parity.Even, Parity.Even)   => Parity.Even
       case _                            => Parity.Top
    }

    /**
     * Over-approximates `bitwise xor`.
     */
    @strict @monotone @commutative @associative
    fn xor(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Parity.Bot)              => Parity.Bot
      case (Parity.Bot, _)              => Parity.Bot
      case (Parity.Odd, Parity.Odd)     => Parity.Even
      case (Parity.Even, Parity.Odd)    => Parity.Odd
      case (Parity.Odd, Parity.Even)    => Parity.Odd
      case (Parity.Even, Parity.Even)   => Parity.Even
      case _                            => Parity.Top
    }

    /**
     * Over-approximates `bitwise left shift`.
     */
    @strict @monotone
    fn leftShift(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Parity.Bot)              => Parity.Bot
      case (Parity.Bot, _)              => Parity.Bot
      case (Parity.Odd, Parity.Odd)     => Parity.Even
      case (Parity.Even, Parity.Odd)    => Parity.Even
      case (Parity.Odd, Parity.Even)    => Parity.Top
      case (Parity.Even, Parity.Even)   => Parity.Even
      case _                            => Parity.Top
    }

    /**
     * Over-approximates `bitwise right shift`.
     */
    @strict @monotone
    fn rightShift(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Parity.Bot)              => Parity.Bot
      case (Parity.Bot, _)              => Parity.Bot
      case _                            => Parity.Top
    }

    /**
      * Over-approximates `equal`.
      */
    @strict @monotone @commutative
    fn eq(e1: Parity, e2: Parity): Belnap/Belnap = match (e1, e2) with {
        case (Parity.Bot, _)            => Belnap/Belnap.Bot
        case (_, Parity.Bot)            => Belnap/Belnap.Bot
        case (Parity.Odd, Parity.Even)  => Belnap/Belnap.False
        case (Parity.Even, Parity.Odd)  => Belnap/Belnap.False
        case _                          => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `not equal`.
      */
    @strict @monotone @commutative
    fn neq(e1: Parity, e2: Parity): Belnap/Belnap = Belnap/not(eq(e1, e2))

    /**
      * Over-approximates `less than`.
      */
    @strict @monotone
    fn less(e1: Parity, e2: Parity): Belnap/Belnap = match (e1, e2) with {
        case (Parity.Bot, _)            => Belnap/Belnap.Bot
        case (_, Parity.Bot)            => Belnap/Belnap.Bot
        case _                          => Belnap/Belnap.Top
    }

    /**
      * Over-approximates `less than or equal`.
      */
    @strict @monotone
    fn lessEqual(e1: Parity, e2: Parity): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Parity<> = (Parity.Bot, Parity.Top, leq, lub, glb)

}