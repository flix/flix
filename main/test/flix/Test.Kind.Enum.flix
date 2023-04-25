mod Test.Kind.Enum {
    mod Implicit {
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

    mod Explicit {


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
            case CBool1(Int32 -> Int32 \ a)
        }

        pub enum ERecordRow[a: RecordRow] {
            case CRecordRow1({ l = Int32 | a })
        }

    }

    @test
    def empty(): Bool = true
}
