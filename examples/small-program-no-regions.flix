use Bool.{==>, <==>}

///
/// A type class for equality and inequality.
///
pub lawful class Eq[a] {

    ///
    /// Returns `true` if and only if `x` is equal to `y`.
    ///
    pub def eq(x: a, y: a): Bool

    ///
    /// Returns `true` if and only if `x` is not equal to `y`.
    ///
    pub def neq(x: a, y: a): Bool = not Eq.eq(x, y)

    ///
    /// Reflexivity: An element `x` is equal to itself.
    ///
    law reflexivity: forall(x: a). x == x

    ///
    /// Symmetry: If `x` is equal to `y` then `y` must also be equal to `x`.
    ///
    law symmetry: forall(x: a, y: a). (x == y) ==> (y == x)

    ///
    /// Transitivity: If `x` is equal to `y` and `y` is equal to `z` then `x` must be equal to `z`.
    ///
    law transitivity: forall(x: a, y: a, z: a). ((x == y) and (y == z)) ==> (x == z)

    ///
    /// x != y is logically equivalent to not (x == y).
    ///
    law inverseNeq: forall(x: a, y: a). (x != y) <==> (not (x == y))
}

instance Eq[Bool] {
    pub def eq(x: Bool, y: Bool): Bool = $BOOL_EQ$(x, y)
    pub override def neq(x: Bool, y: Bool): Bool = $BOOL_NEQ$(x, y)
}

instance Eq[Int32] {
    pub def eq(x: Int32, y: Int32): Bool = $INT16_EQ$(x, y)
    pub override def neq(x: Int32, y: Int32): Bool = $INT16_NEQ$(x, y)
}

///
/// A type class for types that can be converted to strings.
///
pub class ToString[a] {
    ///
    /// Returns a string representation of the given x.
    ///
    pub def toString(x: a): String
}

instance ToString[Bool] {
    pub def toString(x: Bool): String = match x {
        case true  => "true"
        case false => "false"
    }
}

instance ToString[Int32] {
    pub def toString(x: Int32): String =
        import static java.lang.Integer.toString(Int32): String \ {};
        toString(x)
}

pub enum Comparison with Eq {
    case LessThan
    case EqualTo
    case GreaterThan
}

namespace Comparison {

    ///
    /// Converts a `Comparison` value to an integer.
    /// The return value is:
    /// `-1` if `c` is `LessThan`.
    /// `0` if `c` is `EqualTo`.
    /// `+1` if `c` is `GreaterThan`.
    ///
    pub def toInt(c: Comparison): Int32 = match c {
        case LessThan => -1
        case EqualTo => 0
        case GreaterThan => +1
    }

    ///
    /// Converts an integer to a `Comparison` value.
    /// The return value is:
    /// `LessThan` if `i < 0`.
    /// `EqualTo` if `i == 0`.
    /// `GreaterThan` if `i > 0`.
    ///
    pub def fromInt32(i: Int32): Comparison =
        if (i < 0) LessThan
        else if (i == 0) EqualTo
        else GreaterThan

}

///
/// A type class for types with a total order.
///
pub lawful class Order[a] with Eq[a] {

    ///
    /// Returns `LessThan` if `x` < `y`, `Equal` if `x` == `y` or `GreaterThan` `if `x` > `y`.
    ///
    pub def compare(x: a, y: a): Comparison

    ///
    /// Returns `true` if and only if `x < y`.
    ///
    pub def less(x: a, y: a): Bool = match Order.compare(x, y) {
        case LessThan   => true
        case _          => false
    }

    ///
    /// Returns `true` if and only if `x <= y`.
    ///
    pub def lessEqual(x: a, y: a): Bool = match Order.compare(x, y) {
        case LessThan   => true
        case EqualTo    => true
        case _          => false
    }

    ///
    /// Returns `true` if and only if `x > y`.
    ///
    pub def greater(x: a, y: a): Bool = match Order.compare(x, y) {
        case GreaterThan    => true
        case _              => false
    }

    ///
    /// Returns `true` if and only if `x >= y`.
    ///
    pub def greaterEqual(x: a, y: a): Bool = match Order.compare(x, y) {
        case GreaterThan    => true
        case EqualTo        => true
        case _              => false
    }

    ///
    /// Returns the minimum of `x` and `y`.
    ///
    pub def min(x: a, y: a): a = match (x <=> y) {
        case GreaterThan => y
        case _ => x
    }

    ///
    /// Returns the maximum of `x` and `y`.
    ///
    pub def max(x: a, y: a): a = match (x <=> y) {
        case LessThan => y
        case _ => x
    }

    ///
    /// Reflexivity: An element `x` is lower or equal to itself.
    ///
    law reflexivity: forall(x: a). x <= x

    ///
    /// Antisymmetry: If `x` is lower or equal to `y` and `y` is lower or equal to `x` then `x` must be equal to `y`.
    ///
    law symmetry: forall(x: a, y: a). ((x <= y) and (y <= x)) ==> (x == y)

    ///
    /// Transitivity: If `x` is lower or equal to `y` and `y` is lower equal to `z` then `x` must be lower or equal to `z`.
    ///
    law transitivity: forall(x: a, y: a, z: a). ((x <= y) and (y <= z)) ==> (x <= z)

    ///
    /// Totality: For each two elements `x` and `y` either `x` is lower or equal to `y` or the other way round.
    ///
    law totality: forall(x: a, y: a). ((x <= y) or (y <= x))

    ///
    /// Definition of the minimum function.
    ///
    law min: forall(x: a, y: a). ((x <= y) ==> (Order.min(x, y) == x)) and ((y <= x) ==> (Order.min(x, y) == y))

    ///
    /// Definition of the maximum function.
    ///
    law max: forall(x: a, y: a). ((y <= x) ==> (Order.max(x, y) == x)) and ((x <= y) ==> (Order.max(x, y) == y))

    ///
    /// x < y is logically equivalent to not (y <= x).
    /// This law defines the associated strict total order "<" associated with "<=".
    ///
    law strictTotalOrder: forall(x: a, y: a). (x < y) <==> (not (y <= x))

    ///
    /// This law defines ">" based on "<".
    ///
    law inverseOrder1: forall(x: a, y: a). (x > y) <==> (y < x)

    ///
    /// This law defines ">=" based on "<=".
    ///
    law inverseOrder2: forall(x: a, y: a). (x >= y) <==> (y <= x)

    ///
    /// Definition of `compare` based on the defined total order.
    ///
    law compare: forall(x: a, y: a). ((x < y) ==> (Order.compare(x, y) == LessThan)) and ((x == y) ==> (Order.compare(x, y) == EqualTo)) and ((x > y) ==> (Order.compare(x, y) == GreaterThan))
}

namespace Order {

    ///
    /// Returns the minimum of `x` and `y` according to the given comparator `cmp`.
    ///
    pub def minBy(cmp: (a, a) -> Comparison, x: a, y: a): a =
        if (cmp(x, y) == GreaterThan) y else x

    ///
    /// Returns the maximum of `x` and `y` according to the given comparator `cmp`.
    ///
    pub def maxBy(cmp: (a, a) -> Comparison, x: a, y: a): a =
        if (cmp(x, y) == LessThan) y else x

    ///
    /// Lazily combines two comparisons.
    ///
    /// If `c1` is either `LessThan` or `GreaterThan` then `c2` is never evaluated.
    ///
    pub def thenCompare(c1: Comparison, c2: Lazy[Comparison]): Comparison =
        if (c1 != EqualTo) c1 else force c2

}

instance Order[Bool] {

    pub def compare(x: Bool, y: Bool): Comparison =
        if (x) {
            if (y)
                EqualTo
            else
                GreaterThan
        } else {
            if (y)
                LessThan
            else
                EqualTo
        }

}

instance Order[Int32] {

    pub override def less(x: Int32, y: Int32): Bool = $INT32_LT$(x, y)

    pub override def lessEqual(x: Int32, y: Int32): Bool = $INT32_LE$(x, y)

    pub override def greater(x: Int32, y: Int32): Bool = $INT32_GT$(x, y)

    pub override def greaterEqual(x: Int32, y: Int32): Bool = $INT32_GE$(x, y)

    pub def compare(x: Int32, y: Int32): Comparison =
        if ($INT32_LT$(x, y))
            LessThan
        else if ($INT32_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

namespace Bool {

    ///
    /// Alias for logical conjunction.
    ///
    @Time(1) @Space(1)
    pub def ∧(x: Bool, y: Bool): Bool = x and y

    ///
    /// Alias for logical disjunction.
    ///
    @Time(1) @Space(1)
    pub def ∨(x: Bool, y: Bool): Bool = x or y

    ///
    /// Alias for logical implication.
    ///
    @Time(1) @Space(1)
    pub def →(x: Bool, y: Bool): Bool = not x ∨ y

    ///
    /// Alias for logical bi-implication.
    ///
    @Time(1) @Space(1)
    pub def ↔(x: Bool, y: Bool): Bool = (x → y) ∧ (y → x)

    ///
    /// Alias for exclusive or.
    ///
    @Time(1) @Space(1)
    pub def ⊕(x: Bool, y: Bool): Bool = (x ∨ y) ∧ not (x ∧ y)

    ///
    /// Negation of `x`
    ///
    pub def not(x: Bool): Bool = not x

    ///
    /// Logical and.
    ///
    pub def and(x: Bool, y: Lazy[Bool]): Bool = if(x) force y else x

    ///
    /// Logical or.
    ///
    pub def or(x: Bool, y: Lazy[Bool]): Bool = if(x) x else force y

    ///
    /// Returns `true` if `x` implies `y` logically holds.
    ///
    pub def ==>(x: Bool, y: Bool): Bool = not x or y

    ///
    /// Returns true if `x` implies `y` and vise versa.
    ///
    pub def <==>(x: Bool, y: Bool): Bool = x == y

}

///
/// A type class for addition.
///
pub class Add[a] {
    ///
    /// Returns the sum of `x` and `y`.
    ///
    pub def add(x: a, y: a): a
}

instance Add[Int32] {
    pub def add(x: Int32, y: Int32): Int32 = $INT32_ADD$(x, y)
}

///
/// A type class for subtraction.
///
pub class Sub[a] {
    ///
    /// Returns the difference of `x` and `y`.
    ///
    pub def sub(x: a, y: a): a
}

instance Sub[Int32] {
    pub def sub(x: Int32, y: Int32): Int32 = $INT32_SUB$(x, y)
}

///
/// A type class for multiplication.
///
pub class Mul[a] {
    ///
    /// Returns `x` multiplied by `y`.
    ///
    pub def mul(x: a, y: a): a
}

instance Mul[Int32] {
    pub def mul(x: Int32, y: Int32): Int32 = $INT32_MUL$(x, y)
}

///
/// A type class for remainder.
///
pub class Rem[a] {
    ///
    /// Returns the remainder of `x / n`.
    ///
    pub def rem(x: a, n: a): a
}

instance Rem[Int32] {
    pub def rem(x: Int32, n: Int32): Int32 =
        if (n == 0i32)
            0i32
        else
            $INT32_REM$(x, n)
}

///
/// A type class for negation.
///
pub class Neg[a] {
    ///
    /// Returns -`x`.
    ///
    pub def neg(x: a): a
}

instance Neg[Int32] {
    pub def neg(x: Int32): Int32 = $INT32_NEG$(x)
}

///
/// Meta-programming facility to express the time complexity of a function `f`.
///
@Internal
pub def time(_: a -> Int32): Int32 = 1

///
/// Meta-programming facility to express the space complexity of a function `f`.
///
@Internal
pub def space(_: a -> Int32): Int32 = 1

///
/// The identity function.
///
pub def identity(x: a): a = x

///
/// Casts a pure function to `ef`.
///
/// `upcast` is safe to use anywhere.
///
pub def upcast(f: a -> b): a -> b \ ef =
    x -> f(x) as \ ef

///
/// Forwards function composition. Applies the function on the left first.
///
/// Given the functions `f: a -> b` and `g: b -> c` returns a function `a -> c`
///
pub def >>(f: a -> b \ ef1, g: b -> c \ ef2): a -> c \ { ef1, ef2 } = x -> g(f(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `f(x)`.
///
pub def |>(x: a, f: a -> b \ ef): b \ ef = f(x)

namespace Int32 {

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    @Time(1) @Space(1)
    pub def min(x: Int32, y: Int32): Int32 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    @Time(1) @Space(1)
    pub def max(x: Int32, y: Int32): Int32 = if (x >= y) x else y

}

///
/// The Option type.
///
/// An option represents an optional value.
/// The constructor `None` represents an absent value,
/// whereas the constructor `Some(v)` represents the value `v`.
///
pub enum Option[t] with Eq, Order {
    case None,
    case Some(t)
}

///
/// The Result type.
///
/// A result represents a successful value or an error value.
/// The constructor `Ok(v)` represents the successful value `v`,
/// whereas the constructor `Err(v)` represents the error value `v`.
///
pub enum Result[t, e] with Eq, Order, ToString {
    case Ok(t),
    case Err(e)
}

///
/// A type class for types that can be boxed.
///
pub class Boxable[a] with Order[a] {

    /*///
    /// Boxes the given `x`.
    ///
    pub def box(x: a): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as a, o2 as a);
        BoxedObject(value, compare)
    }

    ///
    /// Unboxes the given `x`.
    ///
    pub def unbox(x: Boxed): a = match x {
        case BoxedObject(v, _) => v as a
        case _                 => ?bug
    }*/

}

namespace Result {

    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def flatMap(f: t1 -> Result[t2, e] \ ef, r: Result[t1, e]): Result[t2, e] \ ef = match r {
        case Ok(v)  => f(v)
        case Err(w) => Err(w)
    }

}

///
/// The NonEmpty Chain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports efficient append and "snoc" - appending elements at the tail
/// of the list.
///
/// Note - the constructors `NecOne` and `Nec` should not be used directly.
///
pub enum Nec[t] {
    case NecOne(t)
    case Nec(Nec[t], Nec[t])
}

namespace Nec {

    ///
    /// Return the singleton Nec with element `x`.
    ///
    pub def singleton(x: a): Nec[a] = NecOne(x)

    ///
    /// Add element `x` to the right end of Nec `c`.
    ///
    pub def snoc(c: Nec[a], x: a): Nec[a] = Nec(c, NecOne(x))

}

///
/// The NonEmptyList type.
///
pub enum Nel[a] {
    case Nel(a, List[a])
}

///
/// The Chain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports efficient append and "snoc" - appending elements at the tail
/// of the list.
///
/// Note - the constructors `Empty`, `One` and `Chain` should not be used directly.
///
pub enum Chain[t] {
    case Empty
    case One(t)
    case Chain(Chain[t], Chain[t])
}

namespace Chain {

    ///
    /// Add element `x` to the right end of chain `c`.
    ///
    pub def snoc(c: Chain[a], x: a): Chain[a] = match c {
        case Empty => One(x)
        case _     => Chain(c, One(x))
    }

    ///
    /// Return the empty chain.
    ///
    pub def empty(): Chain[a] = Empty

}

pub enum DelayList[a] {
    case ENil,
    case ECons(a, DelayList[a]),
    case LCons(a, Lazy[DelayList[a]]),
    case LList(Lazy[DelayList[a]])
}

///
/// A type class for types that can be mapped over.
///
pub lawful class Functor[m : Type -> Type] {
    ///
    /// Applies the function `f` to `x` preserving its structure.
    ///
    pub def map(f: a -> b \ ef, x: m[a]): m[b] \ ef

    ///
    /// Mapping the identity function over a functor preserves the functor.
    ///
    law identity: forall(x: m[a]) with Eq[m[a]] . Functor.map(identity, x) == x

    ///
    /// Composing two functions and mapping the resulting function over a functor is the same as
    /// mapping the functions one after the other over the functor.
    ///
    law composition: forall(f: b -> c, g: a -> b, v: m[a]) with Eq[m[c]] . Functor.map(g >> f, v) == Functor.map(f, Functor.map(g, v))
}

namespace Functor {

    ///
    /// `<$>` is an operator alias for `map`.
    ///
    pub def <$>(f: a -> b \ ef, x: m[a]): m[b] \ ef with Functor[m]= Functor.map(f, x)

}

pub lawful class Applicative[m : Type -> Type] with Functor[m] {
    ///
    /// Puts `x` into a default context.
    ///
    pub def point(x: a): m[a]

    ///
    /// Apply the function-type applicative `f` to the argument-type applicative `x`.
    ///
    pub def ap(f: m[a -> b \ ef], x: m[a]): m[b] \ ef

    ///
    /// Lift a binary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Functor.map(f, x1), x2)`).
    ///
    pub def liftA2(f: t1 -> t2 -> r \ ef, x1: m[t1], x2: m[t2]): m[r] \ ef = Applicative.ap(Functor.map(f, x1), x2)

    ///
    /// Lift a ternary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.liftA2(f, x1, x2), x3)`).
    ///
    pub def liftA3(f: t1 -> t2 -> t3 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3]): m[r] \ ef = Applicative.ap(Applicative.liftA2(f, x1, x2), x3)

    ///
    /// Lift a 4-ary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.liftA3(f, x1, x2, x3), x4)`).
    ///
    pub def liftA4(f: t1 -> t2 -> t3 -> t4 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4]): m[r] \ ef = Applicative.ap(Applicative.liftA3(f, x1, x2, x3), x4)

    ///
    /// Lift a 5-ary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.liftA3(f, x1, x2, x3), x4)`).
    ///
    pub def liftA5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4], x5: m[t5]): m[r] \ ef = Applicative.ap(Applicative.liftA4(f, x1, x2, x3, x4), x5)

    ///
    /// Applying the identity function wrapped into an applicative preserves every applicative value.
    ///
    law identity: forall(x: m[a]) with Eq[m[a]] . Applicative.ap(Applicative.point(identity), x) == x

    ///
    /// Applicatively composing two functions and then applicatively applying the resulting function is the same
    /// as applicatively applying the functions one by one.
    ///
    law composition: forall(f: m[b -> c], g: m[a -> b], v: m[a]) with Eq[m[c]] . Applicative.ap(Applicative.ap(Applicative.ap(Applicative.point((f, g) -> g >> f), f), g), v) == Applicative.ap(f, Applicative.ap(g, v))

    ///
    /// `point` is a homomorphism from normal to applicative values regarding application (normal vs. applicative).
    ///
    // TODO error in type checker does not allow to compile this, see #2099
    // law homomorphism: forall(f: a -> b, x: a) with Eq[m[b]] . Applicative.ap(Applicative.point(f), Applicative.point(x)) == Applicative.point(f(x))

    ///
    /// Directly applicatively applying a function `f` to an argument `x` put into a default context is the same as putting the function that takes a function and applies it to `x` into a default context and applicatively applying it to `f`.
    ///
    law interchange: forall(f: m[a -> b], x: a) with Eq[m[b]] . Applicative.ap(f, Applicative.point(x)) == Applicative.ap(Applicative.point(f -> f(x)), f)

    ///
    /// `liftA2` is equivalent to its default implementation.
    ///
    law liftA2Correspondence: forall(f: t1 -> t2 -> r, x1: m[t1], x2: m[t2]) with Eq[m[r]] . Applicative.liftA2(f, x1, x2) == Applicative.ap(Functor.map(f, x1), x2)

    ///
    /// `liftA3` is equivalent to its default implementation.
    ///
    law liftA3Correspondence: forall(f: t1 -> t2 -> t3 -> r, x1: m[t1], x2: m[t2], x3: m[t3]) with Eq[m[r]] . Applicative.liftA3(f, x1, x2, x3) == Applicative.ap(Applicative.liftA2(f, x1, x2), x3)

    ///
    /// `liftA4` is equivalent to its default implementation.
    ///
    law liftA4Correspondence: forall(f: t1 -> t2 -> t3 -> t4 -> r, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4]) with Eq[m[r]] . Applicative.liftA4(f, x1, x2, x3, x4) == Applicative.ap(Applicative.liftA3(f, x1, x2, x3), x4)

    ///
    /// `liftA5` is equivalent to its default implementation.
    ///
    law liftA5Correspondence: forall(f: t1 -> t2 -> t3 -> t4 -> t5 -> r, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4], x5: m[t5]) with Eq[m[r]] . Applicative.liftA5(f, x1, x2, x3, x4, x5) == Applicative.ap(Applicative.liftA4(f, x1, x2, x3, x4), x5)

    ///
    /// Mapping a function over an applicative (with `Functor.map`) is the same as putting the function
    /// into a default context and applying it to the applicative.
    ///
    law mapCorrespondence: forall(f: a -> b, x: m[a]) with Eq[m[b]] . Functor.map(f, x) == Applicative.ap(Applicative.point(f), x)
}

namespace Applicative {

    ///
    /// `<*>` is an operator alias for `ap`.
    ///
    pub def <*>(mf: m[a -> b \ ef], ma: m[a]): m[b] \ ef with Applicative[m] = Applicative.ap(mf, ma)

}

///
/// A type class for types that form a semigroup.
///
pub class SemiGroup[a] {
    ///
    /// An associative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a

    ///
    /// Returns `x` combined with itself `n` times.
    ///
    pub def combineN(x: a, n: Int32): a =
        if (n <= 1) x
        else SemiGroup.combine(x, SemiGroup.combineN(x, n - 1))

    law associative: forall(x: a, y: a, z: a) with Eq[a] . SemiGroup.combine(SemiGroup.combine(x, y), z) == SemiGroup.combine(x, SemiGroup.combine(y, z))

}

pub lawful class Monoid[a] with SemiGroup[a] {
    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a

    ///
    /// Returns the result of combining `x` and `y` using SemiGroup's combine.
    ///
    pub def combine(x: a, y: a): a =
        SemiGroup.combine(x, y)

    law leftIdentity: forall(x: a) with Eq[a] . Monoid.combine(Monoid.empty(), x) == x

    law rightIdentity: forall(x: a) with Eq[a] . Monoid.combine(x, Monoid.empty()) == x

    law associative: forall(x: a, y: a, z: a) with Eq[a] . Monoid.combine(Monoid.combine(x, y), z) == Monoid.combine(x, Monoid.combine(y, z))

}

///
/// A type class for data structures that can be folded.
///
pub class Foldable[t : Type -> Type] {

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, t: t[a]): b \ ef

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, t: t[a]): b \ ef

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty(t: t[a]): Bool =
      Foldable.foldRightWithCont((_, _) -> false, true, t)

    ///
    /// Returns the product of all elements in `t`.
    ///
    pub def product(t: t[Int32]): Int32 =
        if (Foldable.isEmpty(t))
            1
        else
            Foldable.foldLeft((acc, x) -> acc * x, 1, t)

    ///
    /// Returns the product of all elements in `t` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 \ ef, t: t[a]): Int32 \ ef =
        if (Foldable.isEmpty(t))
            1
        else
            Foldable.foldLeft((acc, x) -> acc * f(x), 1, t)

    ///
    /// Returns the sum of all elements in `t`.
    ///
    pub def sum(t: t[Int32]): Int32 =
        Foldable.foldLeft((acc, x) -> acc + x, 0, t)

    ///
    /// Returns the sum of all elements in `t` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, t: t[a]): Int32 \ ef =
        Foldable.foldLeft((acc, x) -> acc + f(x), 0, t)

}

namespace Foldable {

    use Applicative.{*>}

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold[t: Type -> Type, a: Type](t: t[a]): a with Foldable[t], Monoid[a] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, x), Monoid.empty(), t)

}

///
/// The List type.
///
/// A list is either the empty list represented by `Nil`, or
/// an element `v` followed by a list `vs` represented by `v :: vs`.
///
pub enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

instance Foldable[List] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): b \ ef = List.foldLeft(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, l: List[a]): b \ ef = List.foldRightWithCont(f, s, l)
}

instance Functor[List] {
    pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef = List.map(f, l)
}

instance Applicative[List] {
    pub def point(a: a) : List[a] = List.point(a)
    pub def ap(f: List[a -> b \ ef], x: List[a]) : List[b] \ ef = List.ap(f, x)
}

namespace List {

    ///
    /// Returns true if and only if `l` is the empty list, i.e. `Nil`.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(l: List[a]): Bool = match l {
        case Nil => true
        case _   => false
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    @Time(1) @Space(1)
    pub def head(l: List[a]): Option[a] = match l {
        case Nil    => None
        case x :: _ => Some(x)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    @Time(length(l)) @Space(1)
    pub def last(l: List[a]): Option[a] = match l {
        case Nil      => None
        case x :: Nil => Some(x)
        case _ :: rs  => last(rs)
    }

    ///
    /// Returns the length of `l`.
    ///
    @Time(length(l)) @Space(1)
    pub def length(l: List[a]): Int32 =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case _ :: xs => loop(xs, acc + 1)
        };
        loop(l, 0)

    ///
    /// Returns `l2` appended to `l1`.
    ///
    /// The infix operator `:::` is an alias for `append` (`l1 ::: l2 = append(l1, l2)`).
    ///
    @Time(length(l1)) @Space(length(l1))
    pub def append(l1: List[a], l2: List[a]): List[a] =
        foldRight((x, acc) -> x :: acc, l2, l1)

    ///
    /// Returns `true` if and only if `l` contains the element `x`.
    ///
    @Time(length(l)) @Space(1)
    pub def memberOf(a: a, l: List[a]): Bool with Eq[a] = match l {
        case Nil     => false
        case x :: xs => if (a == x) true else memberOf(a, xs)
    }

    ///
    /// Optionally finds the smallest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimum(l: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Optionally finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: List[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Optionally finds the largest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximum(l: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Optionally finds the largest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: List[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Optionally returns the position of `x` in `l`.
    ///
    pub def indexOf(a: a, l: List[a]): Option[Int32] with Eq[a] =
        def loop(ll, acc) = match ll {
            case Nil     => None
            case x :: xs => if (a == x) Some(acc) else loop(xs, acc + 1)
        };
        loop(l, 0)

    ///
    /// Alias for `findLeft`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def find(f: a -> Bool \ ef, l: List[a]): Option[a] \ ef = findLeft(f, l)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findLeft(f: a -> Bool \ ef, l: List[a]): Option[a] \ ef = match l {
        case Nil     => None
        case x :: xs => if (f(x)) Some(x) else findLeft(f, xs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    @Time(e - b) @Space(e - b)
    pub def range(b: Int32, e: Int32): List[Int32] =
        def loop(i, acc) =
            if (i < b)
                acc
            else
                loop(i - 1, i :: acc);
        loop(e - 1, Nil)

    ///
    /// Returns a list with the element `x` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    @Time(n) @Space(n)
    pub def repeat(n: Int32, a: a): List[a] =
        def loop(i, acc) =
            if (i >= n)
                acc
            else
                loop(i + 1, a :: acc);
        loop(0, Nil)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef = scanLeft(f, s, l)

    ///
    /// Accumulates the result of applying `f` to `l` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef =
        def loop(ll, k, acc) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let y = f(acc, x);
                loop(xs, ks -> k(y :: ks), y)
        };
        loop(l, ks -> s :: ks, s)

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => {
                let y = f(x);
                loop(xs, ys -> k(y :: ys))
            }
        };
        loop(l, identity)

    ///
    /// Return the singleton list with element `x`.
    ///
    pub def point(a: a) : List[a] = a :: Nil

    ///
    /// Apply every function from `f` to every argument from `x` and return a list with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: List[a -> b \ ef], x: List[a]) : List[b] \ ef =
        map(g -> map(g, x), f) |> flatten

    ///
    /// Lift a binary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments.
    /// For argument lists `l1 = x1, x2, ...` and `l2 = y1, y2, ...` the results appear in the order
    /// `f(x1,y1), f(x1,y2), ..., f(x2,y1), f(x2,y2), ...`.
    ///
    pub def lift2(f: t1 -> t2 -> r \ ef, l1: List[t1], l2: List[t2]): List[r] \ ef = Applicative.liftA2(f, l1, l2)

    ///
    /// Lift a ternary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments.
    /// For argument lists `l1 = x1, x2, ...`, `l2 = y1, y2, ...` and `l3 = z1, z2, ...` the results appear
    /// in the following order:
    ///
    /// ```
    /// f(x1,y1,z1), f(x1,y1,z2), ..., f(x1,y2,z1), f(x1,y2,z2), ...,
    /// f(x2,y1,z1), f(x2,y1,z2), ..., f(x2,y2,z1), f(x2,y2,z2), ...`
    /// ...
    /// ```
    ///
    pub def lift3(f: t1 -> t2 -> t3 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3]): List[r] \ ef = Applicative.liftA3(f, l1, l2, l3)

    /// Lift a 4-ary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments. The results appear in the order extending the pattern from `lift3`.
    pub def lift4(f: t1 -> t2 -> t3 -> t4 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3], l4: List[t4]): List[r] \ ef = Applicative.liftA4(f, l1, l2, l3, l4)

    /// Lift a 5-ary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments. The results appear in the order extending the pattern from `lift3`.
    pub def lift5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3], l4: List[t4], l5: List[t5]): List[r] \ ef = Applicative.liftA5(f, l1, l2, l3, l4, l5)

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, i, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let y = f(i, x);
                loop(xs, i + 1, ys -> k(y :: ys))
        };
        loop(l, 0, identity)

    ///
    /// Returns the reverse of `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def reverse(l: List[a]): List[a] =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case x :: xs => loop(xs, x :: acc)
        };
        loop(l, Nil)

    ///
    /// Returns `l` with the element at index `i` replaced by `x`.
    ///
    /// Returns `l` if `i < 0` or `i > length(l)-1`.
    ///
    @Time(i) @Space(i)
    pub def update(i: Int32, a: a, l: List[a]): List[a] =
        def loop(ll, j, k) = match (j, ll) {
            case (_, Nil)     => k(Nil)
            case (0, _ :: xs) => k(a :: xs)
            case (_, x :: xs) => loop(xs, j - 1, ks -> k(x :: ks))
        };
        loop(l, i, identity)

    ///
    /// Returns `l` with every occurrence of `from` replaced by `to`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def replace(from: {from = a}, to: {to = a}, l: List[a]): List[a] with Eq[a] = map(e -> if (e == from.from) to.to else e, l)

    ///
    /// Returns `l2` with the `n` elements starting at index `i` replaced with the elements of `l1`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `l2` then no patching is done at these indices.
    /// If `l1` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `l2`, then the element at index `j` in `l1` is used.
    ///
    @Time(n) @Space(length(l2))
    pub def patch(i: Int32, n: Int32, l1: List[a], l2: List[a]): List[a] =
        def loop(ll1, ll2, c, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) => {
                if (c >= i and c < i + n)
                    loop(xs, ys, c + 1, ks -> k(x :: ks))
                else
                    loop(l1, ys, c + 1, ks -> k(y :: ks))
            }
            case _ => k(ll2)
        };
        loop(drop(-i, l1), l2, 0, identity)

    ///
    /// Helper function for `permutations`.
    ///
    def removeIndex(i: Int32, l: List[a]): List[a] = match (i, l) {
        case (_, Nil) => l
        case (0, _ :: xs) => xs
        case (p, x :: xs) => x :: removeIndex(p - 1, xs)
    }

    ///
    /// Helper function for `permutations` and `subsequences`.
    /// Returns `l` with `x` added to the beginning of each element in `l`.
    ///
    def applyHelper(a: a, l: List[List[a]]): List[List[a]] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => loop(xs, ks -> k((a :: x) :: ks))
        };
        loop(l, identity)

    ///
    /// Returns `l` with `x` inserted between every two adjacent elements.
    ///
    @Time(length(l)) @Space(length(l))
    pub def intersperse(a: a, l: List[a]): List[a] =
        def loop(ll, k) = match ll {
            case x1 :: x2 :: xs => loop(x2 :: xs, ks -> k(x1 :: a :: ks))
            case _              => k(ll)
        };
        loop(l, identity)

    ///
    /// Helper function for `transpose`.
    ///
    def uniformHelper(l: List[List[a]], len: Int32): Bool = match l {
        case Nil     => true
        case x :: xs => if (length(x) == len) uniformHelper(xs, len) else false
    }

    ///
    /// Returns `true` if and only if `l1` is a prefix of `l2`.
    ///
    @Time(length(l1)) @Space(1)
    pub def isPrefixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
        case (Nil, _)           => true
        case (_, Nil)           => false
        case (x :: xs, y :: ys) => if (x == y) isPrefixOf(xs, ys) else false
    }

    ///
    /// Returns `true` if and only if `l1` is an infix of `l2`.
    ///
    @Time(length(l1)) @Space(1)
    pub def isInfixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
        case (Nil, _)     => true
        case (_, Nil)     => false
        case (_, _ :: ys) => if (isPrefixOf(l1, l2)) true else isInfixOf(l1, ys)
    }

    ///
    /// Returns `true` if and only if `l1` is a suffix of `l2`.
    ///
    @Time(length(l1)) @Space(Int32.max(length(l1), length(l2)))
    pub def isSuffixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = isPrefixOf(reverse(l1), reverse(l2))

    ///
    /// Returns the result of applying `combine` to all the elements in `l`, using `empty` as the initial value.
    ///
    pub def fold(l: List[a]): a with Monoid[a] = Foldable.fold(l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): b \ ef = match l {
        case Nil     => s
        case x :: xs => foldLeft(f, f(s, x), xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: List[a]): b \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, ks -> k(f(x, ks)))
        };
        loop(l, upcast(identity)) // NB: Cast required because the identity continuation is pure,
                                  // but the intermediate continuations are effect polymorphic

    ///
    /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, l: List[a]): b \ ef = match l {
        case Nil     => z
        case x :: xs => f(x, _ -> foldRightWithCont(f, z, xs))
    }

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, l: List[a]): Option[a] \ ef = match l {
        case Nil     => None
        case x :: xs => Some(foldLeft(f, x, xs))
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def count(f: a -> Bool \ ef, l: List[a]): Int32 \ ef =
        foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, l)

    ///
    /// Returns the product of all elements in the list `l`.
    ///
    pub def product(l: List[Int32]): Int32 =
        Foldable.product(l)

    ///
    /// Returns the product of all elements in the list `l` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 \ ef, l: List[a]): Int32 \ ef =
        Foldable.productWith(f, l)

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def flatten(l: List[List[a]]): List[a] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => loop(xs, ks -> k(x ::: ks))
        };
        loop(l, identity)

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def exists(f: a -> Bool \ ef, l: List[a]): Bool \ ef = match l {
        case Nil     => false
        case x :: xs => if (f(x)) true else exists(f, xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def forall(f: a -> Bool \ ef, l: List[a]): Bool \ ef = match l {
        case Nil     => true
        case x :: xs => if (f(x)) forall(f, xs) else false
    }

    ///
    /// Returns the sublist of `l` without the last element.
    /// Returns `None` if the list `l` is `Nil`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def init(l: List[a]): Option[List[a]] =
        def loop(ll, k) = match ll {
            case Nil      => None
            case _ :: Nil => Some(k(Nil))
            case x :: xs  => loop(xs, ks -> k(x :: ks))
        };
        loop(l, identity)

    ///
    /// Returns the sublist of `l` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// That is, an element at index `i` in `l` is part of the returned sublist if and only if `i >= b` and `i < e`.
    /// Note: Indices that are out of bounds in `l` are not considered (i.e. slice(b, e, l) = slice(max(0,b), min(length(l),e), l)).
    ///
    @Time(e - b) @Space(e - b)
    pub def slice(b: Int32, e: Int32, l: List[a]): List[a] =
        def loop(ll, i, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                if (i < b)
                    loop(xs, i + 1, k)
                else if (i >= e)
                    k(Nil)
                else
                    loop(xs, i + 1, ks -> k(x :: ks))
        };
        if (b < e) loop(l, 0, identity) else Nil

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` contains all elements of `l` that satisfy the predicate `f`.
    /// `l2` contains all elements of `l` that do not satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def partition(f: a -> Bool \ ef, l: List[a]): (List[a], List[a]) \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k((Nil, Nil))
            case x :: xs =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((x :: ks, ls)))
                else
                    loop(xs, match (ks, ls) -> k((ks, x :: ls)))
        };
        loop(l, identity)

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` is the longest prefix of `l` that satisfies the predicate `f`.
    /// `l2` is the remainder of `l`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def span(f: a -> Bool, l: List[a]): (List[a], List[a]) =
        def loop(ll, k) = match ll {
            case Nil     => k((Nil, Nil))
            case x :: xs =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((x :: ks, ls)))
                else
                    k((Nil, ll))
        };
        loop(l, identity)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(l)`.
    /// Returns `l` if `n < 0`.
    ///
    @Time(n) @Space(length(l) - n)
    pub def drop(n: Int32, l: List[a]): List[a] = match l {
        case _ if n <= 0 => l
        case Nil         => Nil
        case _ :: xs     => drop(n - 1, xs)
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def dropWhile(f: a -> Bool \ ef, l: List[a]): List[a] \ ef = match l {
        case Nil     => Nil
        case x :: xs => if (f(x)) dropWhile(f, xs) else l
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Returns `l` if `n > length(l)`.
    /// Returns `Nil` if `n < 0`.
    ///
    @Time(n) @Space(n)
    pub def take(n: Int32, l: List[a]): List[a] =
        def loop(ll, i, k) =
            if (i <= 0)
                k(Nil)
            else
                match ll {
                    case Nil     => k(Nil)
                    case x :: xs => loop(xs, i - 1, ks -> k(x :: ks))
                };
        loop(l, n, identity)

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def takeWhile(f: a -> Bool \ ef, l: List[a]): List[a] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => if (f(x)) loop(xs, ks -> k(x :: ks)) else k(Nil)
        };
        loop(l, identity)

    ///
    /// Split the list `xs` at the position `n` returning the left and right parts.
    /// Position `n` is included in the right part.
    ///
    /// Example: `splitAt(2, 1::2::3::4::Nil)` returns `(1::2::Nil, 3::4::Nil)`
    ///
    /// Returns `(xs, Nil)` if `n > length(xs)`.
    /// Returns `(Nil, xs)` if `n < 0`.
    ///
    pub def splitAt(n: Int32, xs: List[a]): (List[a], List[a]) =
        (List.take(n, xs), List.drop(n, xs))

    ///
    /// Partitions `l` into sublists such that for any two elements `x` and `y` in a sublist, `f(x, y)` is true.
    ///
    /// A sublist is created by iterating through the remaining elements of `l` from left to right and adding an
    /// element to the sublist if and only if doing so creates no conflicts with the elements already in the sublist.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def groupBy(f: (a, a) -> Bool, l: List[a]): List[List[a]] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let (r1, r2) = extractHelper(f, xs, x :: Nil);
                loop(r2, ks -> k(r1 :: ks))
        };
        loop(l, identity)

    ///
    /// Helper function for `groupBy`.
    ///
    /// The list `l2` is needed by `agreeHelper` so it must be materialized (rather than a continuation).
    /// We have no choice but to reverse it.
    ///
    def extractHelper(f: (a, a) -> Bool, l1: List[a], l2: List[a]): (List[a], List[a]) =
        def loop(ll, ll2, k) = match ll {
            case Nil     => (reverse(ll2), k(Nil))
            case x :: xs => if (agreeHelper(f, x, ll2)) loop(xs, x :: ll2, k) else loop(xs, ll2, ks -> k(x :: ks))
        };
        loop(l1, l2, identity)

    ///
    /// Helper function for `groupBy`.
    ///
    def agreeHelper(f: (a, a) -> Bool, a: a, l: List[a]): Bool = match l {
        case Nil     => true
        case x :: xs => if (f(x, a) and f(a, x)) agreeHelper(f, a, xs) else false
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(Int32.min(length(l1), length(l2))) @Space(Int32.min(length(l1), length(l2)))
    pub def zip(l1: List[a], l2: List[b]): List[(a, b)] =
        def loop(ll1, ll2, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) => loop(xs, ys, ks -> k((x, y) :: ks))
            case _                  => k(Nil)
        };
        loop(l1, l2, identity)

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def zipWith(f: (a, b) -> c \ ef, l1: List[a], l2: List[b]): List[c] \ ef =
        def loop(ll1, ll2, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) =>
                let z = f(x, y);
                loop(xs, ys, ks -> k(z :: ks))
            case _ => k(Nil)
        };
        loop(l1, l2, identity)

    ///
    /// Returns a list where each element `e` is mapped to `(e, i)` where `i`
    /// is the index of `e`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def zipWithIndex(l: List[a]): List[(a, Int32)] =
        def loop(ll, i, k) = match ll {
            case Nil       => k(Nil)
            case (x :: xs) => loop(xs, i + 1, ks -> k((x, i) :: ks))
        };
        loop(l, 0, identity)

    ///
    /// Returns a pair of lists, the first containing all first components in `l`
    /// and the second containing all second components in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def unzip(l: List[(a, b)]): (List[a], List[b]) =
        def loop(ll, k) = match ll {
            case Nil            => k((Nil, Nil))
            case (x1, x2) :: xs => loop(xs, match (ks, ls) -> k((x1 :: ks, x2 :: ls)))
        };
        loop(l, identity)

    ///
    /// Returns a list where the element at index `i` is `(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zip3(l1: List[a], l2: List[b], l3: List[c]): List[(a, b, c)] =
        zipWith3((x, y, z) -> (x, y, z), l1, l2, l3)

    ///
    /// Returns a list where the element at index `i` is `f(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith3(f: (a, b, c) -> d \ ef, l1: List[a], l2: List[b], l3: List[c]): List[d] \ ef =
        def loop(ll1, ll2, ll3, k) = match (ll1, ll2, ll3) {
            case (x :: xs, y :: ys, z :: zs) =>
                let r = f(x, y, z);
                loop(xs, ys, zs, ks -> k(r :: ks))
            case _ => k(Nil)
        };
        loop(l1, l2, l3, identity)

    ///
    /// Returns a triple of lists, the first containing all first components in `l`
    /// the second containing all second components in `l` and the third containing all
    /// third components in `l`.
    ///
    pub def unzip3(l: List[(a, b, c)]): (List[a], List[b], List[c]) =
        def loop(ll, k) = match ll {
            case Nil             => k((Nil, Nil, Nil))
            case (x, y, z) :: xs => loop(xs, match (ks, ls, ms) -> k((x :: ks, y :: ls, z :: ms)))
        };
        loop(l, identity)

    ///
    /// Alias for `foldLeft2`.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def fold2(f: (c, a, b) -> c \ ef, c: c, l1: List[a], l2: List[b]): c \ ef = foldLeft2(f, c, l1, l2)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `l1` and `l2`
    /// starting with the initial value `c` and going from left to right.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def foldLeft2(f: (c, a, b) -> c \ ef, c: c, l1: List[a], l2: List[b]): c \ ef = match (l1, l2) {
        case (x :: xs, y :: ys) => foldLeft2(f, f(c, x, y), xs, ys)
        case _                  => c
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, l: List[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), l)

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def filterMap(f: a -> Option[b] \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case None    => loop(xs, k)
                case Some(v) => loop(xs, ks -> k(v :: ks))
            }
        };
        loop(l, identity)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `l`.
    ///
    /// Returns `None` if every element of `l` is `None`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findMap(f: a -> Option[b] \ ef, l: List[a]): Option[b] \ ef = match l {
        case Nil     => None
        case x :: xs => match f(x) {
            case None    => findMap(f, xs)
            case Some(v) => Some(v)
        }
    }

    ///
    /// Returns the elements of `l` as a `DelayList`.
    ///
    pub def toDelayList(l: List[a]): DelayList[a] = match l {
        case Nil     => ENil
        case x :: xs => LCons(x, lazy toDelayList(xs))
    }

    ///
    /// Returns the list `l` as a chain.
    ///
    pub def toChain(l: List[a]): Chain[a] =
        List.foldLeft(Chain.snoc, Chain.empty(), l)

    ///
    /// Applies `f` to every element of `l`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a -> Unit \ ef, l: List[a]): Unit \ ef = match l {
        case Nil     => ()
        case x :: xs => f(x); foreach(f, xs)
    }

    ///
    /// Returns the list `l` as `Option[Nel[a]]`.
    ///
    /// If `l` is empty return `None`, otherwise return the Nel wrapped in `Some`.
    ///
    pub def toNel(l: List[a]): Option[Nel[a]] = match l {
        case Nil     => None
        case x :: xs => Some(Nel(x, xs))
    }

    ///
    /// Returns the list `l` as `Option[Nec[a]]`.
    ///
    /// If `l` is empty return `None`, otherwise return the Nec wrapped in `Some`.
    ///
    pub def toNec(l: List[a]): Option[Nec[a]] = match l {
        case Nil     => None
        case x :: xs => Some(foldLeft(Nec.snoc, Nec.singleton(x), xs))
    }

    ///
    /// Build a list by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new list element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the list.
    ///
    pub def unfold(f: s -> Option[(a, s)] \ ef, st: s): List[a] \ ef =
        def loop(sst, k) = match f(sst) {
            case None         => k(Nil)
            case Some(a, st1) => loop(st1, ks -> k(a :: ks))
        };
        loop(st, identity)

    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a new list element `a`.
    ///
    /// `next` should return `None` to signal the end of building the list.
    ///
    pub def unfoldWithIter(next: Unit -> Option[a] \ ef): List[a] \ ef =
        def loop(k) = match next() {
            case None    => k(Nil)
            case Some(x) => loop(ks -> k(x :: ks))
        };
        loop(identity)

    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Ok(Some(a)` to signal a new list element `Ok(a)`.
    ///
    /// `next` should return `Ok(None)` to signal the end of building the list.
    ///
    /// `next` should return `Err(e)` to signal that an error occurred. The function returns `Err(e)`.
    ///
    pub def unfoldWithOkIter(next: Unit -> Result[Option[a], e] \ ef): Result[List[a], e] \ ef =
        def loop(k) = match next() {
            case Ok(None)    => k(Ok(Nil))
            case Err(e)      => k(Err(e))
            case Ok(Some(x)) => loop(Result.flatMap(ks -> k(Ok(x :: ks))))
        };
        loop(identity)

    ///
    /// Returns the list `l` with duplicates removed. The first occurence of
    /// an element is kept and except for the removal of subsequent duplicates
    /// the order of `l` is preserved.
    ///
    /// `distinct` uses the Flix's builtin equality test. Use `distinctWith` if you
    /// need a custom equality test.
    ///
    pub def distinct(l: List[a]): List[a] with Eq[a] =
        def loop(ll1, ll2, k) = match ll1 {
            case Nil     => k(Nil)
            case x :: xs =>
                if (memberOf(x, ll2))
                    loop(xs, ll2, k)
                else
                    loop(xs, x :: ll2, ks -> k(x :: ks))
        };
        loop(l, Nil, identity)

    ///
    /// Returns the list `l` with duplicates removed using the supplied function
    /// `f` for comparison. The first occurrence of an element is kept and except
    /// for the removal of subsequent duplicates the order of `l` is preserved.
    ///
    pub def distinctWith(f: (a, a) -> Bool, l: List[a]): List[a] =
        def loop(ll1, ll2, k) = match ll1 {
            case Nil     => k(Nil)
            case x :: xs =>
                if (exists(f(x), ll2))
                    loop(xs, ll2, k)
                else
                    loop(xs, x :: ll2, ks -> k(x :: ks))
        };
        loop(l, Nil, identity)

    ///
    /// Returns the sum of all elements in the list `l`.
    ///
    pub def sum(l: List[Int32]): Int32 =
        Foldable.sum(l)

    ///
    /// Returns the sum of all elements in the list `l` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, l: List[a]): Int32 \ ef =
        Foldable.sumWith(f, l)

    ///
    /// Helper function for `sequence` and `traverse`.
    ///
    /// Builds an "applicative list" from a head of one applicative action and an
    /// applicative list of the tail.
    ///
    def consA(mx: f[a], ml: f[List[a]]): f[List[a]] with Applicative[f] =
        use Functor.{<$>};
        use Applicative.{<*>};
        (((x, xs) -> x :: xs) <$> mx) <*> ml

    ///
    /// Returns the result of running all the actions in the list `l` going from left
    /// to right.
    ///
    pub def sequence(l: List[m[a]]): m[List[a]] with Applicative[m] =
        def loop(ll, k) = match ll {
            case Nil      => k(Applicative.point(Nil))
            case mx :: rs => loop(rs, ks -> k(consA(mx, ks)))
        };
        loop(l, identity)

    ///
    /// Merges the two lists `l1` and `l2`. Assuming they are both sorted.
    /// If two elements compare `EqualTo`, then the element of `l1` is first in the result.
    ///
    pub def merge(l1: List[a], l2: List[a]): List[a] with Order[a] =
        def loop(acc1, acc2, k) = match (acc1, acc2) {
            case (x :: xs, y :: ys) => {
                let cmp = x <=> y;
                if (cmp == LessThan or cmp == EqualTo) loop(xs, acc2, ks -> k(x :: ks))
                else                                   loop(acc1, ys, ks -> k(y :: ks))
            }
            case (Nil    , y :: ys) => k(y :: ys)
            case (x :: xs, Nil    ) => k(x :: xs)
            case (Nil    , Nil    ) => k(Nil)
        };
        loop(l1, l2, identity)

}
