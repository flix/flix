namespace Test/Def/Kinding {

    namespace Implicit {

        namespace FormalParams {
            rel R(i: Int)

            pub def star(x: a): Int = ???

            pub def recordRow(x: { l: Int | a }): Int = ???

            pub def schemaRow(x: #{ R | a }): Int = ???

            pub def bool(x: Int -> Int & a): Int = ???

            pub def starToStar(x: a[Int]): Int = ???
        }

        namespace Return {
            rel R(i: Int)

            pub def star(): a = ???
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
            pub enum ERecordRow[_a: RecordRow]
            pub enum ESchemaRow[_a: SchemaRow]

            pub def star(x: EStar[a]): Int = ???

            pub def bool(x: EBool[a]): Int = ???

            pub def record(x: ERecordRow[a]): Int = ???

            pub def schema(x: ESchemaRow[a]): Int = ???

            pub def starStar(x: a, y: EStar[a]): Int = ???
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
        }
    }

    namespace Explicit {

        namespace FormalParams {
            rel R(i: Int)

            pub def star[a: Type](x: a): Int = ???

            pub def recordRow[a: RecordRow](x: { l: Int | a }): Int = ???

            pub def schemaRow[a: SchemaRow](x: #{ R | a }): Int = ???

            pub def bool[a: Bool](x: Int -> Int & a): Int = ???

            pub def starToStar[a: Type -> Type](x: a[Int]): Int = ???
        }

        namespace Return {
            rel R(i: Int)

            pub def star[a: Type](): a = ???
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
            pub enum ERecordRow[_a: RecordRow]
            pub enum ESchemaRow[_a: SchemaRow]

            pub def star[a: Type](x: EStar[a]): Int = ???

            pub def bool[a: Bool](x: EBool[a]): Int = ???

            pub def recordRow[a: RecordRow](x: ERecordRow[a]): Int = ???

            pub def schemaRow[a: SchemaRow](x: ESchemaRow[a]): Int = ???

            pub def starStar[a: Type](x: a, y: EStar[a]): Int = ???
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
    }

    @test
    def empty(): Bool = true
}
