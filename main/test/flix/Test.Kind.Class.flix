namespace Test/Kind/Class {

    namespace Implicit {

        namespace TypeConstraint {
            class CStar[a: Type]

            class CStar1[a] with CStar[a]
        }

        namespace Sig {
            namespace FormalParams {
                class CStar1[a] {
                    pub def star(x: a): Int32 = ???
                }
            }

            namespace Return {
                class CStar1[a] {
                    pub def star(): a = ???
                }
            }

            namespace TypeConstraint {
                class CStar[a: Type]

                class CStar1[a] {
                    pub def star(x: a): Int32 with CStar[a] = ???
                }
            }

            namespace Enum {
                pub enum EStar[_a: Type]

                class CStar1[a] {
                    pub def star(x: EStar[a]): Int32 = ???
                }
            }

            namespace Exp {
                class CStar1[a] {
                    pub def star(x: a): Int32 = let _: a = ???; ???
                }
            }

            namespace Mix {
                // ensure we use `m`'s annotation
                class CTypeBoolType[m: Type -> Bool -> Type] {
                    pub def fAndM(f: a -> b & ef1, x: m[a, ef2]): m[b, ef1 and ef2]
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
                class CStar[a: Type]

                class CStar1[a] {
                    law star: forall(x: a) with CStar[a] . ???
                }
            }

            namespace Enum {
                pub enum EStar[_a: Type]

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
            class CStar[a: Type]
            class CStarToStar[a: Type -> Type]

            class CStar1[a: Type] with CStar[a]

            class CStarToStar1[a: Type -> Type] with CStarToStar[a]
        }

        namespace Sig {
            namespace FormalParams {
                class CStar1[a: Type] {
                    pub def star(x: a): Int32 = ???
                }

                class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: a[Int32]): Int32 = ???
                }
            }

            namespace Return {
                class CStar1[a: Type] {
                    pub def star(): a = ???
                }

                class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(): a[Int32] = ???
                }
            }

            namespace TypeConstraint {
                class CStar[a: Type]
                class CStarToStar[a: Type -> Type]
                class CBoolToStar[a: Bool -> Type]

                class CStar1[a: Type] {
                    pub def star(x: a): Int32 with CStar[a] = ???
                }

                class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: a[Int32]): Int32 with CStarToStar[a] = ???
                }

                class CBoolToStar1[a: Bool -> Type] {
                    pub def boolToStar(x: a[Pure]): Int32 with CBoolToStar[a] = ???
                }
            }

            namespace Enum {
                pub enum EStar[_a: Type]
                pub enum EStarToStar[_a: Type -> Type]

                class CStar1[a: Type] {
                    pub def star(x: EStar[a]): Int32 = ???
                }

                class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: EStarToStar[a]): Int32 = ???
                }
            }

            namespace Exp {
                class CStar1[a: Type] {
                    pub def star(x: a): Int32 = let _: a = ???; ???
                }
            }
        }

        namespace Law {
            namespace FormalParams {
                class CStar1[a: Type] {
                    law star: forall(x: a) . ???
                }

                class CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: a[Int32]) . ???
                }
            }

            namespace TypeConstraint {
                class CStar[a: Type]
                class CStarToStar[a: Type -> Type]
                class CBoolToStar[a: Bool -> Type]

                class CStar1[a: Type] {
                    law star: forall(x: a) with CStar[a] . ???
                }

                class CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: a[Int32]) with CStarToStar[a] . ???
                }

                class CBoolToStar1[a: Bool -> Type] {
                    law boolToStar: forall(x: a[Pure]) with CBoolToStar[a] . ???
                }
            }

            namespace Enum {
                pub enum EStar[_a: Type]
                pub enum EStarToStar[_a: Type -> Type]

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
