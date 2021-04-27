///// We define the elements of the lattice as an enum. The somewhat
///// unorthodox formatting shows the structure of the lattice:
//enum Sign {
//              case Top,
//
//    case Neg, case Zer, case Pos,
//
//              case Bot
//}
//
///// The smallest lattice element.
//instance LowerBound[Sign] {
//    pub def minValue(): Sign = Sign.Bot
//}
//
///// The equality relation on the lattice elements.
//instance Eq[Sign] {
//    pub def eq(x: Sign, y: Sign): Bool = match (x, y) {
//        case (Top, Top) => true
//        case (Neg, Neg) => true
//        case (Zer, Zer) => true
//        case (Pos, Pos) => true
//        case (Bot, Bot) => true
//        case _          => false
//    }
//}
//
///// The partial order relation on the lattice elements.
//instance PartialOrder[Sign] {
//    pub def partialCompare(x: Sign, y: Sign): Bool = match (x, y) {
//        case (Bot, _)   => true
//        case (Neg, Neg) => true
//        case (Zer, Zer) => true
//        case (Pos, Pos) => true
//        case (_, Top)   => true
//        case _          => false
//    }
//}
//
///// The least upper bound relation on the lattice elements.
//instance JoinLattice[Sign] {
//    pub def leastUpperBound(x: Sign, y: Sign): Sign = match (x, y) {
//        case (Bot, _)   => y
//        case (_, Bot)   => x
//        case (Neg, Neg) => Neg
//        case (Zer, Zer) => Zer
//        case (Pos, Pos) => Pos
//        case _          => Top
//    }
//}
//
///// The greatest lower bound relation on the lattice elements.
//instance MeetLattice[Sign] {
//    pub def greatestLowerBound(e1: Sign, e2: Sign): Sign = match (e1, e2) {
//        case (Top, x)   => x
//        case (x, Top)   => x
//        case (Neg, Neg) => Neg
//        case (Zer, Zer) => Zer
//        case (Pos, Pos) => Pos
//        case _          => Bot
//    }
//}
//
//// We want a toString instance to pretty print the result.
//instance ToString[Sign] {
//    pub def toString(x: Sign): String = match x {
//        case Top => "Top"
//        case Neg => "Neg"
//        case Zer => "Zer"
//        case Pos => "Pos"
//        case Bot => "Bot"
//    }
//}
//
//// For technical reasons, we must have a boxable instance.
//instance Boxable[Sign] {
//    pub def box(x: Sign): Boxed = {
//        let value = x as ##java.lang.Object;
//        let compare = (o1, o2) -> Order.compare(o1 as Sign, o2 as Sign);
//        let toString = o -> ToString.toString(o as Sign);
//        BoxedObject(value, compare, toString)
//    }
//    pub def unbox(x: Boxed): Sign = match x {
//        case BoxedObject(v, _, _) => v as Sign
//        case _                    => ?bug
//    }
//}
//
//// The Boxable instance requires an Order instance (different from the partial order).
//instance Order[Sign] {
//    pub def compare(x: Sign, y: Sign): Comparison = match (x, y) {
//        case (Bot, Bot) => EqualTo
//        case (Neg, Neg) => EqualTo
//        case (Zer, Zer) => EqualTo
//        case (Pos, Pos) => EqualTo
//        case (Top, Top) => EqualTo
//        case (Bot, _)   => LessThan
//        case (Neg, _)   => LessThan
//        case (Zer, _)   => LessThan
//        case (Pos, _)   => LessThan
//        case _          => GreaterThan
//    }
//}
//
///// We can now declare three map lattices:
//lat A(x: String, s: Sign)
//lat B(x: String, s: Sign)
//lat R(x: String, s: Sign)
//
//// We can now put everything together.
//def main(_: Array[String]): Int32 & Impure =
//    let p = #{
//        /// We can write facts with lattice semantics:
//        A("a"; Pos).
//        B("a"; Top).
//        A("b"; Neg).
//
//        /// And rules with lattice semantics:
//        R("c"; s) :- A("a"; s).
//        R("c"; s) :- A("b"; s).
//        R("d"; s) :- A(x; s), B(x; s).
//    };
//    let m = solve p;
//    (m |> Fixpoint.unsafePrint) as & Impure;
//    0
