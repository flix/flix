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

        pub enum ERecordRow[a: RecordRow] {
            case CRecordRow1({ l = Int32 | a })
        }

        pub enum ESchemaRow[a: SchemaRow] {
            case CSchemaRow1(#{ R(Int32) | a })
        }

        pub enum EEff[a: Eff] {
            case CEff1(Int32 -> Int32 \ a)
        }

        pub enum EStarToStar[a: Type -> Type] {
            case CStarToStar1(a[Int32])
        }

        pub enum EEffToStar[a: Eff -> Type] {
            case CEffToStar1(a[{}])
        }

        pub enum EBoolToStar[a: Bool -> Type] {
            case CBoolToStar1(a[true])
        }

        pub enum ERecordRowToStar[a: RecordRow -> Type] {
            case CRecordRowToStar1(a[()])
        }

        pub enum ESchemaRowToStar[a: SchemaRow -> Type] {
            case CSchemaRowToStar1(a[#()])
        }
    }

    @test
    def empty(): Bool = true
}
