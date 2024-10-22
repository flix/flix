mod Test.Kind.Trait {

    mod Implicit {

        mod TypeConstraint {
            trait CStar[a: Type]

            trait CStar1[a] with CStar[a]
        }

        mod Sig {
            mod FormalParams {
                trait CStar1[a] {
                    pub def star(x: a): Int32 = ???
                }
            }

            mod Return {
                trait CStar1[a] {
                    pub def star(): a = ???
                }
            }

            mod TypeConstraint {
                trait CStar[a: Type]

                trait CStar1[a] {
                    pub def star(x: a): Int32 with CStar[a] = ???
                }
            }

            mod Enum {
                pub enum EStar[_a: Type]

                trait CStar1[a] {
                    pub def star(x: EStar[a]): Int32 = ???
                }
            }

            mod Exp {
                trait CStar1[a] {
                    pub def star(x: a): Int32 = let _: a = ???; ???
                }
            }

            mod Mix {
                // ensure we use `m`'s annotation
                trait CTypeBoolType[m: Type -> Eff -> Type] {
                    pub def fAndM(f: a -> b \ ef1, x: m[a, ef2]): m[b, ef1 + ef2]
                }
            }
        }

        mod Law {
            mod FormalParams {
                trait CStar1[a] {
                    law star: forall(x: a) ???
                }
            }

            mod TypeConstraint {
                trait CStar[a: Type]

                trait CStar1[a] {
                    law star: forall(x: a) with CStar[a] ???
                }
            }

            mod Enum {
                pub enum EStar[_a: Type]

                trait CStar1[a] {
                    law star: forall(x: EStar[a]) ???
                }
            }

            mod Exp {
                trait CStar1[a] {
                    law star: forall(x: a) { (???: a); ??? }
                }
            }
        }
    }

    mod Explicit {
        mod TypeConstraint {
            trait CStar[a: Type]
            trait CStarToStar[a: Type -> Type]

            trait CStar1[a: Type] with CStar[a]

            trait CStarToStar1[a: Type -> Type] with CStarToStar[a]
        }

        mod Sig {
            mod FormalParams {
                trait CStar1[a: Type] {
                    pub def star(x: a): Int32 = ???
                }

                trait CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: a[Int32]): Int32 = ???
                }
            }

            mod Return {
                trait CStar1[a: Type] {
                    pub def star(): a = ???
                }

                trait CStarToStar1[a: Type -> Type] {
                    pub def starToStar(): a[Int32] = ???
                }
            }

            mod TypeConstraint {
                trait CStar[a: Type]
                trait CStarToStar[a: Type -> Type]
                trait CBoolToStar[a: Eff -> Type]

                trait CStar1[a: Type] {
                    pub def star(x: a): Int32 with CStar[a] = ???
                }

                trait CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: a[Int32]): Int32 with CStarToStar[a] = ???
                }

                trait CBoolToStar1[a: Eff -> Type] {
                    pub def boolToStar(x: a[{}]): Int32 with CBoolToStar[a] = ???
                }
            }

            mod Enum {
                pub enum EStar[_a: Type]
                pub enum EStarToStar[_a: Type -> Type]

                trait CStar1[a: Type] {
                    pub def star(x: EStar[a]): Int32 = ???
                }

                trait CStarToStar1[a: Type -> Type] {
                    pub def starToStar(x: EStarToStar[a]): Int32 = ???
                }
            }

            mod Exp {
                trait CStar1[a: Type] {
                    pub def star(x: a): Int32 = let _: a = ???; ???
                }
            }
        }

        mod Law {
            mod FormalParams {
                trait CStar1[a: Type] {
                    law star: forall(x: a) ???
                }

                trait CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: a[Int32]) ???
                }
            }

            mod TypeConstraint {
                trait CStar[a: Type]
                trait CStarToStar[a: Type -> Type]
                trait CBoolToStar[a: Eff -> Type]

                trait CStar1[a: Type] {
                    law star: forall(x: a) with CStar[a] ???
                }

                trait CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: a[Int32]) with CStarToStar[a] ???
                }

                trait CBoolToStar1[a: Eff -> Type] {
                    law boolToStar: forall(x: a[{}]) with CBoolToStar[a] ???
                }
            }

            mod Enum {
                pub enum EStar[_a: Type]
                pub enum EStarToStar[_a: Type -> Type]

                trait CStar1[a: Type] {
                    law star: forall(x: EStar[a]) ???
                }

                trait CStarToStar1[a: Type -> Type] {
                    law starToStar: forall(x: EStarToStar[a]) ???
                }
            }

            mod Exp {
                trait CStar1[a: Type] {
                    law star: forall(x: a) { (???: a); ??? }
                }
            }
        }
    }

    @test
    def empty(): Bool = true
}
