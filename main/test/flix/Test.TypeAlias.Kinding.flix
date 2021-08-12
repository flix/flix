namespace Test/Enum/Kinding {
    namespace Implicit {
        type alias TStar[a] = a

        type alias EStarStar[a, b] = (a, b)
    }

    namespace Explicit {
        rel R(i: Int)

        type alias TStar[a: Type] = a

        type alias TStarStar[a: Type, b: Type] = (a, b)

        type alias TBool[a: Bool] = Int -> Int & a

        type alias TRecord[a: Record] = { l: Int | a }

        type alias TSchema[a: Schema] = #{ R | a }

        type alias TStarToStar[a: Type -> Type] = a[Int]

        type alias TBoolToStar[a: Bool -> Type] = a[Pure]

        type alias TRecordToStar[a: Record -> Type] = a[{}]

        type alias TSchemaToStar[a: Schema -> Type] = a[#{}]
    }

    @test
    def empty(): Bool = true
}