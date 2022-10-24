namespace Test/Exp/ReifyType {

    @test
    def matchTypeUnit01(): Bool = matchType () {
        case _: Unit => true
        case _: _ => false
    }

    @test
    def matchTypeUnit02(): Bool = matchType () {
        case x: Unit => x == ()
        case _: _ => false
    }

    @test
    def matchTypeBool01(): Bool = matchType true {
        case _: Bool => true
        case _: _ => false
    }

    @test
    def matchTypeBool02(): Bool = matchType true {
        case x: Bool => x == true
        case _: _ => false
    }

    @test
    def matchTypeChar01(): Bool = matchType 'a' {
        case _: Char => true
        case _: _ => false
    }

    @test
    def matchTypeChar02(): Bool = matchType 'a' {
        case x: Char => x == 'a'
        case _: _ => false
    }

    @test
    def matchTypeFloat32_01(): Bool = matchType 0.123f32 {
        case _: Float32 => true
        case _: _ => false
    }

    @test
    def matchTypeFloat32_02(): Bool = matchType 0.123f32 {
        case x: Float32 => x == 0.123f32
        case _: _ => false
    }

    @test
    def matchTypeFloat64_01(): Bool = matchType 0.123f64 {
        case _: Float64 => true
        case _: _ => false
    }

    @test
    def matchTypeFloat64_02(): Bool = matchType 0.123f64 {
        case x: Float64 => x == 0.123f64
        case _: _ => false
    }

    @test
    def matchTypeInt8_01(): Bool = matchType 123i8 {
        case _: Int8 => true
        case _: _ => false
    }

    @test
    def matchTypeInt8_02(): Bool = matchType 123i8 {
        case x: Int8 => x == 123i8
        case _: _ => false
    }

    @test
    def matchTypeInt16_01(): Bool = matchType 123i16 {
        case _: Int16 => true
        case _: _ => false
    }

    @test
    def matchTypeInt16_02(): Bool = matchType 123i16 {
        case x: Int16 => x == 123i16
        case _: _ => false
    }

    @test
    def matchTypeInt32_01(): Bool = matchType 123i32 {
        case _: Int32 => true
        case _: _ => false
    }

    @test
    def matchTypeInt32_02(): Bool = matchType 123i32 {
        case x: Int32 => x == 123i32
        case _: _ => false
    }

    @test
    def matchTypeInt64_01(): Bool = matchType 123i64 {
        case _: Int64 => true
        case _: _ => false
    }

    @test
    def matchTypeInt64_02(): Bool = matchType 123i64 {
        case x: Int64 => x == 123i64
        case _: _ => false
    }

    @test
    def matchTypeBigInt01(): Bool = matchType 123ii {
        case _: BigInt => true
        case _: _ => false
    }

    @test
    def matchTypeBigInt02(): Bool = matchType 123ii {
        case x: BigInt => x == 123ii
        case _: _ => false
    }

    @test
    def matchTypeString01(): Bool = matchType "hello" {
        case _: String => true
        case _: _ => false
    }

    @test
    def matchTypeString02(): Bool = matchType "hello" {
        case x: String => x == "hello"
        case _: _ => false
    }

    @test
    def matchTypeArrayUnit01(): Bool \ IO = matchType [()] {
        case _: Array[Unit, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayUnit02(): Bool \ IO = matchType [()] {
        case x: Array[Unit, Static] => Array.sameElements(x, [()])
        case _: _ => false
    }

    @test
    def matchTypeArrayBool01(): Bool \ IO = matchType [true] {
        case _: Array[Bool, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayBool02(): Bool \ IO = matchType [true] {
        case x: Array[Bool, Static] => Array.sameElements(x, [true])
        case _: _ => false
    }

    @test
    def matchTypeArrayChar01(): Bool \ IO = matchType ['a'] {
        case _: Array[Char, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayChar02(): Bool \ IO = matchType ['a'] {
        case x: Array[Char, Static] => Array.sameElements(x, ['a'])
        case _: _ => false
    }

    @test
    def matchTypeArrayFloat32_01(): Bool \ IO = matchType [0.123f32] {
        case _: Array[Float32, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayFloat32_02(): Bool \ IO = matchType [0.123f32] {
        case x: Array[Float32, Static] => Array.sameElements(x, [0.123f32])
        case _: _ => false
    }

    @test
    def matchTypeArrayFloat64_01(): Bool \ IO = matchType [0.123f64] {
        case _: Array[Float64, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayFloat64_02(): Bool \ IO = matchType [0.123f64] {
        case x: Array[Float64, Static] => Array.sameElements(x, [0.123f64])
        case _: _ => false
    }

    @test
    def matchTypeArrayInt8_01(): Bool \ IO = matchType [123i8] {
        case _: Array[Int8, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayInt8_02(): Bool \ IO = matchType [123i8] {
        case x: Array[Int8, Static] => Array.sameElements(x, [123i8])
        case _: _ => false
    }

    @test
    def matchTypeArrayInt16_01(): Bool \ IO = matchType [123i16] {
        case _: Array[Int16, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayInt16_02(): Bool \ IO = matchType [123i16] {
        case x: Array[Int16, Static] => Array.sameElements(x, [123i16])
        case _: _ => false
    }

    @test
    def matchTypeArrayInt32_01(): Bool \ IO = matchType [123i32] {
        case _: Array[Int32, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayInt32_02(): Bool \ IO = matchType [123i32] {
        case x: Array[Int32, Static] => Array.sameElements(x, [123i32])
        case _: _ => false
    }

    @test
    def matchTypeArrayInt64_01(): Bool \ IO = matchType [123i64] {
        case _: Array[Int64, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayInt64_02(): Bool \ IO = matchType [123i64] {
        case x: Array[Int64, Static] => Array.sameElements(x, [123i64])
        case _: _ => false
    }

    @test
    def matchTypeArrayBigInt01(): Bool \ IO = matchType [123ii] {
        case _: Array[BigInt, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayBigInt02(): Bool \ IO = matchType [123ii] {
        case x: Array[BigInt, Static] => Array.sameElements(x, [123ii])
        case _: _ => false
    }

    @test
    def matchTypeArrayString01(): Bool \ IO = matchType ["hello"] {
        case _: Array[String, Static] => true
        case _: _ => false
    }

    @test
    def matchTypeArrayString02(): Bool \ IO = matchType ["hello"] {
        case x: Array[String, Static] => Array.sameElements(x, ["hello"])
        case _: _ => false
    }

    @test
    def matchTypeFallThrough01(): Bool = matchType "hello" {
        case x: Int8 => x == 123i8
        case x: Int64 => x == 123i64
        case _: _ => true
    }

    @test
    def matchTypePolymorphic(): Bool = matchType Nil {
        case _: List[Unit] => false
        case _: List[_] => true
    }
}
