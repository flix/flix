mod Test.Kind.TypeAlias {
    mod Implicit {
        type alias TStar[a] = a

        type alias EStarStar[a, b] = (a, b)
    }

    mod Explicit {
        type alias TStar[a: Type] = a

        type alias TStarStar[a: Type, b: Type] = (a, b)

        type alias TBool[a: Bool] = Int32 -> Int32 \ a

        type alias TRecordRow[a: RecordRow] = { l = Int32 | a }

        type alias TSchemaRow[a: SchemaRow] = #{ R(Int32) | a }

        type alias TStarToStar[a: Type -> Type] = a[Int32]

        type alias TBoolToStar[a: Bool -> Type] = a[Pure]

        type alias TRecordRowToStar[a: RecordRow -> Type, r: RecordRow] = a[r]

        type alias TSchemaRowToStar[a: SchemaRow -> Type, r: SchemaRow] = a[r]
    }

    @test
    def empty(): Bool = true
}
