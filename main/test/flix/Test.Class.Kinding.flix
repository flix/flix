// MATT move to better namespace
class CStarToStar[a: Type -> Type]
class CBoolToStar[a: Bool -> Type]
class CRecordToStar[a: Record -> Type]
class CSchemaToStar[a: Schema -> Type]
class CStar[a: Type]
class CBool[a: Bool]
class CRecord[a: Record]
class CSchema[a: Schema]
enum EStarToStar[a: Type -> Type]
enum EBoolToStar[a: Bool -> Type]
enum ERecordToStar[a: Record -> Type]
enum ESchemaToStar[a: Schema -> Type]
enum EStar[a: Type]
enum EBool[a: Bool]
enum ERecord[a: Record]
enum ESchema[a: Schema]
rel R(i: Int)

namespace Test/Class/Kinding {

    namespace Implicit {

        namespace TypeConstraint {
            class CStar1[a] with CStar[a]
        }

        namespace Sig {
            namespace FormalParams {
                lawless class CStar1[a] {
                    pub def star(x: a): Int = ???
                }
            }

            namespace Return {
                lawless class CStar1[a] {
                    pub def star(): a = ???
                }
            }

            namespace TypeConstraint {
                lawless class CStar1[a] {
                    pub def star(x: a): Int with CStar[a] = ???
                }
            }

            namespace Enum {
                lawless class CStar1[a] {
                    pub def star(x: EStar[a]): Int = ???
                }
            }

            namespace Exp {
                lawless class CStar1[a] {
                    pub def star(x: a): Int = ???: a; ???
                }
            }
        }

        namespace Law {
            namespace FormalParams {
                class CStar1[a] {
                    law star: forall(x: a) . ???
                }
            }

            namespace TypeConstraint {
                 class CStar1[a] {
                    law star: forall(x: a) with CStar[a] . ???
                }
            }

            namespace Enum {
                 class CStar1[a] {
                    law star: forall(x: EStar[a]) . ???
                }
            }

            namespace Exp {
                 class CStar1[a] {
                    law star: forall(x: a) . ???: a; ???
                }
            }
        }

        namespace FormalParams {
            pub def star(x: a): Int = ???

            pub def record(x: { l: Int | a }): Int = ???

            pub def schema(x: #{ R | a }): Int = ???

            pub def bool(x: Int -> Int & a): Int = ???

            pub def starToStar(x: a[Int]): Int = ???

            pub def starRecord(x: a, y: { l: Int | a }): Int = ???

            pub def starSchema(x: a, y: #{ R | a }): Int = ???
        }

        namespace Return {
            pub def star(): a = ???

            pub def recordStar(x: { l: Int | a }): a = ???

            pub def schemaStar(x: #{ R | a }): a = ???
        }

        namespace TypeConstraint {
            pub def star(x: a): Int with CStar[a] = ???

            pub def starToStar(x: a[Int]): Int with CStarToStar[a] = ???
        }

        namespace Enum {
            pub def star(x: EStar[a]): Int = ???

            pub def bool(x: EBool[a]): Int = ???

            pub def record(x: ERecord[a]): Int = ???

            pub def schema(x: ESchema[a]): Int = ???

            pub def starStar(x: a, y: EStar[a]): Int = ???

            pub def starRecord(x: a, y: EStar[a]): Int = ???

            pub def starSchema(x: a, y: EStar[a]): Int = ???
        }

        namespace Effect {
            pub def bool(): Int & ef = ???

            pub def func(x: Int -> Int & ef): Int & ef = ???

            pub def enum(x: EBool[ef]): Int & ef = ???
        }

        namespace Exp {
            pub def star(x: a): Int = ???: a; ???

            pub def record(x: { l: Int | a }): Int = ???: a; ???

            pub def schema(x: #{ R | a }): Int = ???: a; ???
        }
    }

    namespace Explicit {

        namespace FormalParams {
            pub def star[a: Type](x: a): Int = ???

            pub def record[a: Record](x: { l: Int | a }): Int = ???

            pub def schema[a: Schema](x: #{ R | a }): Int = ???

            pub def bool[a: Bool](x: Int -> Int & a): Int = ???

            pub def starToStar[a: Type -> Type](x: a[Int]): Int = ???

            pub def starRecord[a: Record](x: a, y: { l: Int | a }): Int = ???

            pub def starSchema[a: Schema](x: a, y: #{ R | a }): Int = ???
        }

        namespace Return {
            pub def star[a: Type](): a = ???

            pub def recordStar[a: Record](x: { l: Int | a }): a = ???

            pub def schemaStar[a: Schema](x: #{ R | a }): a = ???
        }

        namespace TypeConstraint {
            pub def star[a: Type](x: a): Int with CStar[a] = ???

            pub def starToStar[a: Type -> Type](x: a[Int]): Int with CStarToStar[a] = ???

            // cannot be inferred
            pub def boolToStar[a: Bool -> Type, e: Bool](x: a[e]): Int & e with CBoolToStar[a] = ???
        }

        namespace Enum {
            pub def star[a: Type](x: EStar[a]): Int = ???

            pub def bool[a: Bool](x: EBool[a]): Int = ???

            pub def record[a: Record](x: ERecord[a]): Int = ???

            pub def schema[a: Schema](x: ESchema[a]): Int = ???

            pub def starStar[a: Type](x: a, y: EStar[a]): Int = ???

            pub def starRecord[a: Record](x: a, y: ERecord[a]): Int = ???

            pub def starSchema[a: Schema](x: a, y: ESchema[a]): Int = ???
        }

        namespace Effect {
            pub def bool[ef: Bool](): Int & ef = ???

            pub def func[ef: Bool](x: Int -> Int & ef): Int & ef = ???

            pub def enum[ef: Bool](x: EBool[ef]): Int & ef = ???
        }

        namespace Exp {
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
