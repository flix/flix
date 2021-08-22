namespace Test/Enum/Kinding {
    namespace Implicit {
        pub enum EStar[a] {
            case CStar1(a)
            case CStar2(a -> a)
        }

        pub enum EStarStar[a, b] {
            case CStarStar1(a)
            case CStarStar2(b)
            case CStarStar3(a -> b)
        }
    }

    namespace Explicit {
        rel R(i: Int)

        pub enum EStar[a: Type] {
            case CStar1(a)
            case CStar2(a -> a)
        }

        pub enum EStarStar[a: Type, b: Type] {
            case CStarStar1(a)
            case CStarStar2(b)
            case CStarStar3(a -> b)
        }

        pub enum EBool[a: Bool] {
            case CBool1(Int -> Int & a)
        }

        pub enum ERecord[a: Record] {
            case CRecord1(a)
            case CRecord2({ l: Int | a })
        }

        pub enum ESchema[a: Schema] {
            case CSchema1(a)
            case CSchema2(#{ R | a })
        }

        // These pass kinding but cause a crash in the Finalizer.
//        pub enum EStarToStar[a: Type -> Type] {
//            case CStarToStar1(a[Int])
//        }
//
//        pub enum EBoolToStar[a: Bool -> Type] {
//            case CBoolToStar1(a[Pure])
//        }
//
//        pub enum ERecordToStar[a: Record -> Type] {
//            case CRecordToStar1(a[{}])
//        }
//
//        pub enum ESchemaToStar[a: Schema -> Type] {
//            case CSchemaToStar1(a[#{}])
//        }
    }

    @test
    def empty(): Bool = true
}