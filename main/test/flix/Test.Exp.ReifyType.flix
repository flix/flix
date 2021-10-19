namespace Test/Exp/ReifyType {

    @test
    def reify01(): Bool = reifyType a == ErasedType

    @test
    def reify02(): Bool = reifyType Bool == ReifiedBool

    @test
    def reify03(): Bool = reflect(true) == ReifiedBool

    @test
    def reify04(): Bool = reifyType Char == ReifiedChar

    @test
    def reify05(): Bool = reflect('a') == ReifiedChar

    @test
    def reify06(): Bool = reifyType Float32 == ReifiedFloat32

    @test
    def reify07(): Bool = reflect(1.0f32) == ReifiedFloat32

    @test
    def reify08(): Bool = reifyType Float64 == ReifiedFloat64

    @test
    def reify09(): Bool = reflect(1.0f64) == ReifiedFloat64

    @test
    def reify10(): Bool = reifyType Int8 == ReifiedInt8

    @test
    def reify11(): Bool = reflect(1i8) == ReifiedInt8

    @test
    def reify12(): Bool = reifyType Int16 == ReifiedInt16

    @test
    def reify13(): Bool = reflect(1i16) == ReifiedInt16

    @test
    def reify14(): Bool = reifyType Int32 == ReifiedInt32

    @test
    def reify15(): Bool = reflect(1) == ReifiedInt32

    @test
    def reify16(): Bool = reifyType Int64 == ReifiedInt64

    @test
    def reify17(): Bool = reflect(1i64) == ReifiedInt64

    @test
    def reify18(): Bool = reifyType Array[Int32] == ReifiedArray(ReifiedInt32)

    @test
    def reify19(): Bool = reflect([1]) == ReifiedArray(ReifiedInt32)

    @test
    def reify20(): Bool = reifyType Array[a] == ReifiedArray(ErasedType)

    @test
    def reify21(): Bool = reflect([]: Array[a]) == ReifiedArray(ErasedType)

    def reflect(x: a): ReifiedType = reifyType a

}
