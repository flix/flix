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

        pub enum ESchemaRow[a: SchemaRow] {
            case CSchemaRow1(#{ R(Int32) | a })
        }

        // TODO disabled during development
//        pub enum EEffect[a: Eff] {
//            case CEffect1(Int32 -> Int32 \ a)
//        }

        // These pass kinding but cause a crash in the Finalizer.
//        pub enum EStarToStar[a: Type -> Type] {
//            case CStarToStar1(a[Int32])
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
