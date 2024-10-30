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
            trait CStar[a: Type]
            trait CStarToStar[a: Type -> Type]

            pub def star(x: a): Int32 with CStar[a] = ???

            pub def starToStar(x: a[Int32]): Int32 with CStarToStar[a] = ???
        }

        mod Enum {
            pub enum EStar[_a: Type]
            pub enum EBool[_a: Eff]
            pub enum ERecordRow[_a: RecordRow]
            pub enum ESchemaRow[_a: SchemaRow]

            pub def star(x: EStar[a]): Int32 = ???

            pub def bool(x: EBool[a]): Int32 = ???

            pub def record(x: ERecordRow[a]): Int32 = ???

            pub def schema(x: ESchemaRow[a]): Int32 = ???

            pub def starStar(x: a, y: EStar[a]): Int32 = ???
        }

        mod Effect {
            pub enum EBool[_a: Eff]

            pub def bool(): Int32 \ ef = ???()

            pub def func(x: Int32 -> Int32 \ ef): Int32 \ ef = ???()

            pub def enum_(x: EBool[ef]): Int32 \ ef = ???()
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

            pub def bool[a: Eff](x: Int32 -> Int32 \ a): Int32 = ???

            pub def starToStar[a: Type -> Type](x: a[Int32]): Int32 = ???
        }

        mod Return {
            pub def star[a: Type](): a = ???
        }

        mod TypeConstraint {
            trait CStar[a: Type]
            trait CStarToStar[a: Type -> Type]
            trait CBoolToStar[a: Eff -> Type]

            pub def star[a: Type](x: a): Int32 with CStar[a] = ???

            pub def starToStar[a: Type -> Type](x: a[Int32]): Int32 with CStarToStar[a] = ???

            // cannot be inferred
            pub def boolToStar[a: Eff -> Type, ef: Eff](x: a[ef]): Int32 \ ef with CBoolToStar[a] = ???()
        }

        mod Enum {
            pub enum EStar[_a: Type]
            pub enum EBool[_a: Eff]
            pub enum ERecordRow[_a: RecordRow]
            pub enum ESchemaRow[_a: SchemaRow]

            pub def star[a: Type](x: EStar[a]): Int32 = ???

            pub def bool[a: Eff](x: EBool[a]): Int32 = ???

            pub def recordRow[a: RecordRow](x: ERecordRow[a]): Int32 = ???

            pub def schemaRow[a: SchemaRow](x: ESchemaRow[a]): Int32 = ???

            pub def starStar[a: Type](x: a, y: EStar[a]): Int32 = ???
        }

        mod Effect {
            pub enum EBool[_a: Eff]

            pub def bool[ef: Eff](): Int32 \ ef = ???()

            pub def func[ef: Eff](x: Int32 -> Int32 \ ef): Int32 \ ef = ???()

            pub def enum_[ef: Eff](x: EBool[ef]): Int32 \ ef = ???()
        }

        mod Exp {
            pub def star[a: Type](x: a): Int32 = let _: a = ???; ???
        }
    }

    mod Regression {
        mod Test01 {
            pub enum Func[ef : Eff] {
                case Function(Int32 -> Int32 \ ef)
            }
            pub def func(f: Int32 -> Int32 \ ef, g: Func[ef]): Int32 = ???
        }

        mod Test02 {
            pub def project3(p: PredSym, ts: f[(t1, t2, t3)]): Datalog[v] with Order[t1], Order[t2], Order[t3], Foldable[f] = ???

            pub enum PredSym
            pub enum Datalog[_a]
            pub trait Foldable[a: Type -> Type]
        }
    }

    @test
    def empty(): Bool = true
}
