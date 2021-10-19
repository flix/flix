namespace Test/Exp/ReifyType {

    @test
    def reify01(): Bool = reifyType Bool == ReifiedBool

    @test
    def reify02(): Bool = reflect(true) == ReifiedBool

    @test
    def reify03(): Bool = reifyType Char == ReifiedChar

    @test
    def reify04(): Bool = reflect('a') == ReifiedChar

    @test
    def reify05(): Bool = reifyType Float32 == ReifiedFloat32

    @test
    def reify06(): Bool = reflect(1.0f32) == ReifiedFloat32

    // Look into `Prelude.flix` for the full types available in `ReifiedType`.

    //def reflect(x: a): ReifiedType = reifyType a

/*

case ErasedType,
case ReifiedBool,
case ReifiedChar,
case ReifiedFloat32,
case ReifiedFloat64,
case ReifiedInt8,
case ReifiedInt16,
case ReifiedInt32,
case ReifiedInt64,
case ReifiedArray(ReifiedType)

*/
}
