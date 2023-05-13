mod Test.Kind.Def {

    mod Implicit {

        mod FormalParams {
            pub def star(x: a): Int32 = ???

            pub def recordRow(x: { l = Int32 | a }): Int32 = ???

            pub def schemaRow(x: #{ R(Int32) | a }): Int32 = ???

            pub def bool(x: Int32 -> Int32 \ a): Int32 = ???

            pub def starToStar(x: a[Int32]): Int32 = ???
        }

        mod Return {
            pub def star(): a = ???
        }

        mod TypeConstraint {
            class CStar[a: Type]
            class CStarToStar[a: Type -> Type]

            pub def star(x: a): Int32 with CStar[a] = ???

            pub def starToStar(x: a[Int32]): Int32 with CStarToStar[a] = ???
        }

        mod Enum {
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

        mod Effect {
            pub enum EBool[_a: Bool]

            pub def bool(): Int32 \ ef = unchecked_cast(??? as _ \ ef)

            pub def func(x: Int32 -> Int32 \ ef): Int32 \ ef = unchecked_cast(??? as _ \ ef)

            pub def enum_(x: EBool[ef]): Int32 \ ef = unchecked_cast(??? as _ \ ef)
        }

        mod Exp {
            pub def star(x: a): Int32 = let _: a = ???; ???
        }
    }

    mod Explicit {

        mod FormalParams {
            pub def star[a: Type](x: a): Int32 = ???

            pub def recordRow[a: RecordRow](x: { l = Int32 | a }): Int32 = ???

            pub def schemaRow[a: SchemaRow](x: #{ R(Int32) | a }): Int32 = ???

            pub def bool[a: Bool](x: Int32 -> Int32 \ a): Int32 = ???

            pub def starToStar[a: Type -> Type](x: a[Int32]): Int32 = ???
        }

        mod Return {
            pub def star[a: Type](): a = ???
        }

        mod TypeConstraint {
            class CStar[a: Type]
            class CStarToStar[a: Type -> Type]
            class CBoolToStar[a: Bool -> Type]

            pub def star[a: Type](x: a): Int32 with CStar[a] = ???

            pub def starToStar[a: Type -> Type](x: a[Int32]): Int32 with CStarToStar[a] = ???

            // cannot be inferred
            pub def boolToStar[a: Bool -> Type, ef: Bool](x: a[ef]): Int32 \ ef with CBoolToStar[a] = unchecked_cast(??? as _ \ ef)
        }

        mod Enum {
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

        mod Effect {
            pub enum EBool[_a: Bool]

            pub def bool[ef: Bool](): Int32 \ ef = unchecked_cast(??? as _ \ ef)

            pub def func[ef: Bool](x: Int32 -> Int32 \ ef): Int32 \ ef = unchecked_cast(??? as _ \ ef)

            pub def enum_[ef: Bool](x: EBool[ef]): Int32 \ ef = unchecked_cast(??? as _ \ ef)
        }

        mod Exp {
            pub def star[a: Type](x: a): Int32 = let _: a = ???; ???
        }
    }

    mod Regression {
        mod Test01 {
            pub enum Func[ef : Bool] {
                case Function(Int32 -> Int32 \ ef)
            }
            pub def func(f: Int32 -> Int32 \ ef, g: Func[ef]): Int32 = ???
        }

        mod Test02 {
            pub def project3(p: PredSym, ts: f[(t1, t2, t3)]): Datalog[v] with Order[t1], Order[t2], Order[t3], Foldable[f] = ???

            pub enum PredSym
            pub enum Datalog[_a]
            pub class Foldable[a: Type -> Type]
        }
    }

    @test
    def empty(): Bool = true
}
