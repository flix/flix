mod Test.Kind.Class {

    mod Implicit {

        mod TypeConstraint {
            class CStar[a: Type]

            class CStar1[a] with CStar[a]
        }

        mod Sig {
            mod FormalParams {
                class CStar1[a] {
                    pub def star(x: a): Int32 = ???
                }
            }

            mod Return {
                class CStar1[a] {
                    pub def star(): a = ???
                }
            }

            mod TypeConstraint {
                class CStar[a: Type]

                class CStar1[a] {
                    pub def star(x: a): Int32 with CStar[a] = ???
                }
            }

            mod Enum {
                pub enum EStar[_a: Type]

                class CStar1[a] {
                    pub def star(x: EStar[a]): Int32 = ???
                }
            }

            mod Exp {
                class CStar1[a] {
                    pub def star(x: a): Int32 = let _: a = ???; ???
                }
            }

            mod Mix {
                // ensure we use `m`'s annotation
                class CTypeBoolType[m: Type -> Bool -> Type] {
                    pub def fAndM(f: a -> b \ ef1, x: m[a, ef2]): m[b, ef1 + ef2]
                }
            }
        }

        mod Law {
            mod FormalParams {
                class CStar1[a] {
                    law star: forall(x: a) . ???
                }
            }

            mod TypeConstraint {
                class CStar[a: Type]

                class CStar1[a] {
                    law star: forall(x: a) with CStar[a] . ???
                }
            }

            mod Enum {
                pub enum EStar[_a: Type]

                class CStar1[a] {
                    law star: forall(x: EStar[a]) . ???
                }
            }

            mod Exp {
                class CStar1[a] {
                    law star: forall(x: a) . { ???: a; ??? }
                }
            }
        }
    }

    mod Explicit {
        mod TypeConstraint {
            class CStar[a: Type]
            class CStarToStar[a: Type -> Type]

            class CStar1[a: Type] with CStar[a]

            class CStarToStar1[a: Type -> Type] with CStarToStar[a]
        }

        mod Sig {
            mod FormalParams {
                class CStar1[a: Type] {
                    pub def star(x: a): Int32 = ???
                }

                class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: a[Int32]): Int32 = ???
                }
            }

            mod Return {
                class CStar1[a: Type] {
                    pub def star(): a = ???
                }

                class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(): a[Int32] = ???
                }
            }

            mod TypeConstraint {
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

            mod Enum {
                pub enum EStar[_a: Type]
                pub enum EStarToStar[_a: Type -> Type]

                class CStar1[a: Type] {
                    pub def star(x: EStar[a]): Int32 = ???
                }

                class CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: EStarToStar[a]): Int32 = ???
                }
            }

            mod Exp {
                class CStar1[a: Type] {
                    pub def star(x: a): Int32 = let _: a = ???; ???
                }
            }
        }

        mod Law {
            mod FormalParams {
                class CStar1[a: Type] {
                    law star: forall(x: a) . ???
                }

                class CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: a[Int32]) . ???
                }
            }

            mod TypeConstraint {
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

            mod Enum {
                pub enum EStar[_a: Type]
                pub enum EStarToStar[_a: Type -> Type]

                class CStar1[a: Type] {
                    law star: forall(x: EStar[a]) . ???
                }

                class CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: EStarToStar[a]) . ???
                }
            }

            mod Exp {
                class CStar1[a: Type] {
                    law star: forall(x: a) . { ???: a; ??? }
                }
            }
        }
    }

    @test
    def empty(): Bool = true
}
