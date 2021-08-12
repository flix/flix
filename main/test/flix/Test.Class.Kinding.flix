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
                    law star: forall(x: a) . { ???: a; ??? }
                }
            }
        }
    }

    namespace Explicit {
        namespace TypeConstraint {
            class CStar1[a: Type] with CStar[a]

            class CStarToStar1[a: Type -> Type] with CStarToStar[a]
        }

        namespace Sig {
            namespace FormalParams {
                lawless class CStar1[a: Type] {
                    pub def star(x: a): Int = ???
                }

                lawless class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: a[Int]): Int = ???
                }
            }

            namespace Return {
                lawless class CStar1[a: Type] {
                    pub def star(): a = ???
                }

                lawless class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(): a[Int] = ???
                }
            }

            namespace TypeConstraint {
                lawless class CStar1[a: Type] {
                    pub def star(x: a): Int with CStar[a] = ???
                }

                lawless class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: a[Int]): Int with CStarToStar[a] = ???
                }

                lawless class CBoolToStar1[a: Bool -> Type] {
                    pub def boolToStar(x: a[Pure]): Int with CBoolToStar[a] = ???
                }
            }

            namespace Enum {
                lawless class CStar1[a: Type] {
                    pub def star(x: EStar[a]): Int = ???
                }

                lawless class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: EStarToStar[a]): Int = ???
                }
            }

            namespace Exp {
                lawless class CStar1[a: Type] {
                    pub def star(x: a): Int = ???: a; ???
                }
            }
        }

        namespace Law {
            namespace FormalParams {
                class CStar1[a: Type] {
                    law star: forall(x: a) . ???
                }

                class CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: a[Int]) . ???
                }
            }

            namespace TypeConstraint {
                class CStar1[a: Type] {
                    law star: forall(x: a) with CStar[a] . ???
                }

                class CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: a[Int]) with CStarToStar[a] . ???
                }

                class CBoolToStar1[a: Bool -> Type] {
                    law boolToStar: forall(x: a[Pure]) with CBoolToStar[a] . ???
                }
            }

            namespace Enum {
                class CStar1[a: Type] {
                    law star: forall(x: EStar[a]) . ???
                }

                class CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: EStarToStar[a]) . ???
                }
            }

            namespace Exp {
                class CStar1[a: Type] {
                    law star: forall(x: a) . { ???: a; ??? }
                }
            }
        }
    }

    @test
    def empty(): Bool = true
}
