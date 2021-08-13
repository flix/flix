namespace Test/Def/Kinding {

    namespace Implicit {

        namespace FormalParams {
            rel R(i: Int)

            pub def star(x: a): Int = ???

            pub def record(x: { l: Int | a }): Int = ???

            pub def schema(x: #{ R | a }): Int = ???

            pub def bool(x: Int -> Int & a): Int = ???

            pub def starToStar(x: a[Int]): Int = ???

            pub def starRecord(x: a, y: { l: Int | a }): Int = ???

            pub def starSchema(x: a, y: #{ R | a }): Int = ???
        }

        namespace Return {
            rel R(i: Int)

            pub def star(): a = ???

            pub def recordStar(x: { l: Int | a }): a = ???

            pub def schemaStar(x: #{ R | a }): a = ???
        }

        namespace TypeConstraint {
            class CStar[a: Type]
            class CStarToStar[a: Type -> Type]

            pub def star(x: a): Int with CStar[a] = ???

            pub def starToStar(x: a[Int]): Int with CStarToStar[a] = ???
        }

        namespace Enum {
            pub enum EStar[_a: Type]
            pub enum EBool[_a: Bool]
            pub enum ERecord[_a: Record]
            pub enum ESchema[_a: Schema]

            pub def star(x: EStar[a]): Int = ???

            pub def bool(x: EBool[a]): Int = ???

            pub def record(x: ERecord[a]): Int = ???

            pub def schema(x: ESchema[a]): Int = ???

            pub def starStar(x: a, y: EStar[a]): Int = ???

            pub def starRecord(x: a, y: ERecord[a]): Int = ???

            pub def starSchema(x: a, y: ESchema[a]): Int = ???
        }

        namespace Effect {
            pub enum EBool[_a: Bool]

            pub def bool(): Int & ef = ???

            pub def func(x: Int -> Int & ef): Int & ef = ???

            pub def enum(x: EBool[ef]): Int & ef = ???
        }

        namespace Exp {
            rel R(i: Int)

            pub def star(x: a): Int = ???: a; ???

            pub def record(x: { l: Int | a }): Int = ???: a; ???

            pub def schema(x: #{ R | a }): Int = ???: a; ???
        }
    }

    namespace Explicit {

        namespace FormalParams {
            rel R(i: Int)

            pub def star[a: Type](x: a): Int = ???

            pub def record[a: Record](x: { l: Int | a }): Int = ???

            pub def schema[a: Schema](x: #{ R | a }): Int = ???

            pub def bool[a: Bool](x: Int -> Int & a): Int = ???

            pub def starToStar[a: Type -> Type](x: a[Int]): Int = ???

            pub def starRecord[a: Record](x: a, y: { l: Int | a }): Int = ???

            pub def starSchema[a: Schema](x: a, y: #{ R | a }): Int = ???
        }

        namespace Return {
            rel R(i: Int)

            pub def star[a: Type](): a = ???

            pub def recordStar[a: Record](x: { l: Int | a }): a = ???

            pub def schemaStar[a: Schema](x: #{ R | a }): a = ???
        }

        namespace TypeConstraint {
            class CStar[a: Type]
            class CStarToStar[a: Type -> Type]
            class CBoolToStar[a: Bool -> Type]

            pub def star[a: Type](x: a): Int with CStar[a] = ???

            pub def starToStar[a: Type -> Type](x: a[Int]): Int with CStarToStar[a] = ???

            // cannot be inferred
            pub def boolToStar[a: Bool -> Type, e: Bool](x: a[e]): Int & e with CBoolToStar[a] = ???
        }

        namespace Enum {
            pub enum EStar[_a: Type]
            pub enum EBool[_a: Bool]
            pub enum ERecord[_a: Record]
            pub enum ESchema[_a: Schema]

            pub def star[a: Type](x: EStar[a]): Int = ???

            pub def bool[a: Bool](x: EBool[a]): Int = ???

            pub def record[a: Record](x: ERecord[a]): Int = ???

            pub def schema[a: Schema](x: ESchema[a]): Int = ???

            pub def starStar[a: Type](x: a, y: EStar[a]): Int = ???

            pub def starRecord[a: Record](x: a, y: ERecord[a]): Int = ???

            pub def starSchema[a: Schema](x: a, y: ESchema[a]): Int = ???
        }

        namespace Effect {
            pub enum EBool[_a: Bool]

            pub def bool[ef: Bool](): Int & ef = ???

            pub def func[ef: Bool](x: Int -> Int & ef): Int & ef = ???

            pub def enum[ef: Bool](x: EBool[ef]): Int & ef = ???
        }

        namespace Exp {
            rel R(i: Int)

            pub def star[a: Type](x: a): Int = ???: a; ???

            pub def record[a: Record](x: { l: Int | a }): Int = ???: a; ???

            pub def schema[a: Schema](x: #{ R | a }): Int = ???: a; ???
        }
    }

    namespace Regression {
        namespace Test01 {
            pub enum Func[ef : Bool] {
                case Function(Int -> Int & ef)
            }
            pub def func(f: Int -> Int & ef, g: Func[ef]): Int = ???
        }

        namespace Test02 {
            pub def project3(p: PredSym, ts: f[(t1, t2, t3)]): Datalog[v] with Boxable[t1], Boxable[t2], Boxable[t3], Foldable[f] = ???

            pub enum PredSym
            pub class Boxable[a]
            pub enum Datalog[_a]
            pub class Foldable[a: Type -> Type]
        }

        namespace Test03 {
            pub enum E[a: Record] {
                case C(a)
            }
        }
    }

    @test
    def empty(): Bool = true
}
