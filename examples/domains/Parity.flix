//
// An implementation of the parity domain.
//
namespace domains.parity {

    ///
    /// The elements of the parity lattice.
    ///
    enum Parity {
                  case Top,

        case Odd,          case Even,

                  case Bot
    }

    ///
    /// Returns `true` iff `e1` is less than or equal to `e2`.
    ///
    def leq(e1: Parity, e2: Parity): Bool = match (e1, e2) with {
        case (Bot, _)     => true
        case (Odd, Odd)   => true
        case (Even, Even) => true
        case (_, Top)     => true
        case _            => false
    }

    ///
    /// Returns the least upper bound of `e1` and `e2`.
    ///
    @commutative @associative
    def lub(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Bot, x)     => x
        case (x, Bot)     => x
        case (Odd, Odd)   => Odd
        case (Even, Even) => Even
        case _            => Top
    }

    ///
    /// Returns the greatest lower bound of `e1` and `e2`.
    ///
    @commutative @associative
    def glb(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Top, x)     => x
        case (x, Top)     => x
        case (Odd, Odd)   => Odd
        case (Even, Even) => Even
        case _            => Bot
    }

    ///
    /// Abstracts a concrete integer into the Parity domain.
    ///
    def alpha(i: Int): Parity = if (i % 2 != 0) Odd else Even

    ///
    /// Over-approximates integer `increment`.
    ///
    @strict @monotone
    def inc(e: Parity): Parity = match e with {
        case Bot  => Bot
        case Odd  => Even
        case Even => Odd
        case Top  => Top
    }

    ///
    /// Over-approximates integer `decrement`.
    ///
    @strict @monotone
    def dec(e: Parity): Parity = match e with {
        case Bot  => Bot
        case Odd  => Even
        case Even => Odd
        case Top  => Top
    }

    ///
    /// Over-approximates integer `addition`.
    ///
    @strict @monotone @commutative @associative
    def plus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Bot)       => Bot
        case (Bot, _)       => Bot
        case (Odd, Odd)     => Even
        case (Odd, Even)    => Odd
        case (Even, Odd)    => Odd
        case (Even, Even)   => Even
        case _              => Top
    }

    ///
    /// Over-approximates integer `subtraction`.
    ///
    @strict @monotone
    def minus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Bot)       => Bot
        case (Bot, _)       => Bot
        case (Odd, Odd)     => Even
        case (Odd, Even)    => Odd
        case (Even, Odd)    => Odd
        case (Even, Even)   => Even
        case _              => Top
    }

    ///
    /// Over-approximates integer `multiplication`.
    ///
    @strict @monotone @commutative @associative
    def times(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Bot)       => Bot
        case (Bot, _)       => Bot
        case (Odd, Odd)     => Odd
        case (Odd, Even)    => Even
        case (Even, Odd)    => Even
        case (Even, Even)   => Even
        case _              => Top
    }

    ///
    /// Over-approximates integer `division`.
    ///
    @strict @monotone
    def divide(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
       case (_, Bot)    => Bot
       case (Bot, _)    => Bot
       case _           => Top
    }

    ///
    /// Over-approximates integer `modulus`.
    ///
    @strict @monotone
    def modulo(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
       case (_, Bot)        => Bot
       case (Bot, _)        => Bot
       case (Odd, Odd)      => Top
       case (Even, Odd)     => Top
       case (Odd, Even)     => Odd
       case (Even, Even)    => Even
       case _               => Top
    }

    ///
    /// Over-approximates `bitwise negation`.
    ///
    @strict @monotone
    def negate(e: Parity): Parity = match e with {
        case Bot    => Bot
        case Odd    => Even
        case Even   => Odd
        case Top    => Top
    }

    ///
    /// Over-approximates `bitwise and`.
    ///
    @strict @monotone @commutative @associative
    def and(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
       case (_, Bot)        => Bot
       case (Bot, _)        => Bot
       case (Odd, Odd)      => Odd
       case (Even, Odd)     => Even
       case (Odd, Even)     => Even
       case (Even, Even)    => Even
       case _               => Top
    }

    ///
    /// Over-approximates `bitwise or`.
    ///
    @strict @monotone @commutative @associative
    def or(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Bot)         => Bot
      case (Bot, _)         => Bot
      case (Odd, Odd)       => Odd
      case (Even, Odd)      => Odd
      case (Odd, Even)      => Odd
      case (Even, Even)     => Even
      case _                => Top
    }

    ///
    /// Over-approximates `bitwise xor`.
    ///
    @strict @monotone @commutative @associative
    def xor(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Bot)         => Bot
      case (Bot, _)         => Bot
      case (Odd, Odd)       => Even
      case (Even, Odd)      => Odd
      case (Odd, Even)      => Odd
      case (Even, Even)     => Even
      case _                => Top
    }

    ///
    /// Over-approximates `bitwise left shift`.
    ///
    @strict @monotone
    def leftShift(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Bot)         => Bot
      case (Bot, _)         => Bot
      case (Odd, Odd)       => Even
      case (Even, Odd)      => Even
      case (Odd, Even)      => Top
      case (Even, Even)     => Even
      case _                => Top
    }

    ///
    /// Over-approximates `bitwise right shift`.
    ///
    @strict @monotone
    def rightShift(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
      case (_, Bot)     => Bot
      case (Bot, _)     => Bot
      case _            => Top
    }

    ///
    /// Over-approximates `equal`.
    ///
    @strict @monotone @commutative
    def eq(e1: Parity, e2: Parity): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)       => Belnap/Belnap.Bot
        case (_, Bot)       => Belnap/Belnap.Bot
        case (Odd, Even)    => Belnap/Belnap.False
        case (Even, Odd)    => Belnap/Belnap.False
        case _              => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `not equal`.
    ///
    @strict @monotone @commutative
    def neq(e1: Parity, e2: Parity): Belnap/Belnap = Belnap/not(eq(e1, e2))

    ///
    /// Over-approximates `less than`.
    ///
    @strict @monotone
    def less(e1: Parity, e2: Parity): Belnap/Belnap = match (e1, e2) with {
        case (Bot, _)   => Belnap/Belnap.Bot
        case (_, Bot)   => Belnap/Belnap.Bot
        case _          => Belnap/Belnap.Top
    }

    ///
    /// Over-approximates `less than or equal`.
    ///
    @strict @monotone
    def lessEqual(e1: Parity, e2: Parity): Belnap/Belnap = Belnap/or(e1 `less` e2, e1 `eq` e2)

    let Parity<> = (Bot, Top, leq, lub, glb)

}