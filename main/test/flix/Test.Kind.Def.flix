namespace Test/Kind/Def {

    namespace Implicit {

        namespace FormalParams {
            rel R(i: Int32)

            pub def star(x: a): Int32 = ???

            pub def recordRow(x: { l :: Int32 | a }): Int32 = ???

            pub def schemaRow(x: #{ R | a }): Int32 = ???

            pub def bool(x: Int32 -> Int32 & a): Int32 = ???

            pub def starToStar(x: a[Int32]): Int32 = ???
        }

        namespace Return {
            rel R(i: Int32)

            pub def star(): a = ???
        }

        namespace TypeConstraint {
            class CStar[a: Type]
            class CStarToStar[a: Type -> Type]

            pub def star(x: a): Int32 with CStar[a] = ???

            pub def starToStar(x: a[Int32]): Int32 with CStarToStar[a] = ???
        }

        namespace Enum {
            pub enum EStar[_a: Type]
            pub enum EBool[_a: Bool]
            pub enum ERecordRow[_a: RecordRow]
            pub enum ESchemaRow[_a: SchemaRow]

            pub def star(x: EStar[a]): Int32 = ???

            pub def bool(x: EBool[a]): Int32 = ???

            pub def record(x: ERecordRow[a]): Int32 = ???

            pub def schema(x: ESchemaRow[a]): Int32 = ???

            pub def starStar(x: a, y: EStar[a]): Int32 = ???
        }

        namespace Effect {
            pub enum EBool[_a: Bool]

            pub def bool(): Int32 & ef = ??? as & ef

            pub def func(x: Int32 -> Int32 & ef): Int32 & ef = ??? as & ef

            pub def enum_(x: EBool[ef]): Int32 & ef = ??? as & ef
        }

        namespace Exp {
            rel R(i: Int32)

            pub def star(x: a): Int32 = let _: a = ???; ???
        }
    }

    namespace Explicit {

        namespace FormalParams {
            rel R(i: Int32)

            pub def star[a: Type](x: a): Int32 = ???

            pub def recordRow[a: RecordRow](x: { l :: Int32 | a }): Int32 = ???

            pub def schemaRow[a: SchemaRow](x: #{ R | a }): Int32 = ???

            pub def bool[a: Bool](x: Int32 -> Int32 & a): Int32 = ???

            pub def starToStar[a: Type -> Type](x: a[Int32]): Int32 = ???
        }

        namespace Return {
            rel R(i: Int32)

            pub def star[a: Type](): a = ???
        }

        namespace TypeConstraint {
            class CStar[a: Type]
            class CStarToStar[a: Type -> Type]
            class CBoolToStar[a: Bool -> Type]

            pub def star[a: Type](x: a): Int32 with CStar[a] = ???

            pub def starToStar[a: Type -> Type](x: a[Int32]): Int32 with CStarToStar[a] = ???

            // cannot be inferred
            pub def boolToStar[a: Bool -> Type, ef: Bool](x: a[ef]): Int32 & ef with CBoolToStar[a] = ??? as & ef
        }

        namespace Enum {
            pub enum EStar[_a: Type]
            pub enum EBool[_a: Bool]
            pub enum ERecordRow[_a: RecordRow]
            pub enum ESchemaRow[_a: SchemaRow]

            pub def star[a: Type](x: EStar[a]): Int32 = ???

            pub def bool[a: Bool](x: EBool[a]): Int32 = ???

            pub def recordRow[a: RecordRow](x: ERecordRow[a]): Int32 = ???

            pub def schemaRow[a: SchemaRow](x: ESchemaRow[a]): Int32 = ???

            pub def starStar[a: Type](x: a, y: EStar[a]): Int32 = ???
        }

        namespace Effect {
            pub enum EBool[_a: Bool]

            pub def bool[ef: Bool](): Int32 & ef = ??? as & ef

            pub def func[ef: Bool](x: Int32 -> Int32 & ef): Int32 & ef = ??? as & ef

            pub def enum_[ef: Bool](x: EBool[ef]): Int32 & ef = ??? as & ef
        }

        namespace Exp {
            rel R(i: Int32)

            pub def star[a: Type](x: a): Int32 = let _: a = ???; ???
        }
    }

    namespace Regression {
        namespace Test01 {
            pub enum Func[ef : Bool] {
                case Function(Int32 -> Int32 & ef)
            }
            pub def func(f: Int32 -> Int32 & ef, g: Func[ef]): Int32 = ???
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
