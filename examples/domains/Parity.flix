/**
 * An implementation of the parity lattice.
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
    fn glb(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Parity.Top, x)                => x
        case (x, Parity.Top)                => x
        case (Parity.Odd, Parity.Odd)       => Parity.Odd
        case (Parity.Even, Parity.Even)     => Parity.Even
        case _                              => Parity.Bot
    }

    /**
     * Over-approximates integer `addition`.
     */
    @strict @mono
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
    @strict @mono
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
    @strict @mono
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
    @strict @mono
    fn divide(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
       case (_, Parity.Bot)              => Parity.Bot
       case (Parity.Bot, _)              => Parity.Bot
       case _                            => Parity.Top
    }

    /**
     * Over-approximates integer `modulus`.
     */
    @strict @mono
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
    @strict @mono
    fn negate(e: Parity): Parity = match e with {
        case Parity.Bot     =>  Parity.Bot
        case Parity.Odd     =>  Parity.Even
        case Parity.Even    =>  Parity.Odd
        case Parity.Top     =>  Parity.Top
    }

    /**
     * Over-approximates `bitwise or`.
     */
    @strict @mono
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
    @strict @mono
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
    @strict @mono
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
    @strict @mono
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
    @strict @mono
    fn rightShift(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Parity.Bot)              => Parity.Bot
      case (Parity.Bot, _)              => Parity.Bot
      case _                            => Parity.Top
    }

    // TODO: Add commutative annotation?

}