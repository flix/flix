namespace Test/Exp/ReifyType {

    @test
    def reifyUnit01(): Bool = matchType () {
        case _: Unit => true
        case _: _ => false
    }

    @test
    def reifyUnit02(): Bool = matchType () {
        case x: Unit => x == ()
        case _: _ => false
    }

    @test
    def reifyBool01(): Bool = matchType true {
        case _: Bool => true
        case _: _ => false
    }

    @test
    def reifyBool02(): Bool = matchType true {
        case x: Bool => x == true
        case _: _ => false
    }

    @test
    def reifyChar01(): Bool = matchType 'a' {
        case _: Char => true
        case _: _ => false
    }

    @test
    def reifyChar02(): Bool = matchType 'a' {
        case x: Char => x == 'a'
        case _: _ => false
    }

    @test
    def reifyFloat32_01(): Bool = matchType 0.123f32 {
        case _: Float32 => true
        case _: _ => false
    }

    @test
    def reifyFloat32_02(): Bool = matchType 0.123f32 {
        case x: Float32 => x == 0.123f32
        case _: _ => false
    }

    @test
    def reifyFloat64_01(): Bool = matchType 0.123f64 {
        case _: Float64 => true
        case _: _ => false
    }

    @test
    def reifyFloat64_02(): Bool = matchType 0.123f64 {
        case x: Float64 => x == 0.123f64
        case _: _ => false
    }

    @test
    def reifyInt8_01(): Bool = matchType 123i8 {
        case _: Int8 => true
        case _: _ => false
    }

    @test
    def reifyInt8_02(): Bool = matchType 123i8 {
        case x: Int8 => x == 123i8
        case _: _ => false
    }

    @test
    def reifyInt16_01(): Bool = matchType 123i16 {
        case _: Int16 => true
        case _: _ => false
    }

    @test
    def reifyInt16_02(): Bool = matchType 123i16 {
        case x: Int16 => x == 123i16
        case _: _ => false
    }

    @test
    def reifyInt32_01(): Bool = matchType 123i32 {
        case _: Int32 => true
        case _: _ => false
    }

    @test
    def reifyInt32_02(): Bool = matchType 123i32 {
        case x: Int32 => x == 123i32
        case _: _ => false
    }

    @test
    def reifyInt64_01(): Bool = matchType 123i64 {
        case _: Int64 => true
        case _: _ => false
    }

    @test
    def reifyInt64_02(): Bool = matchType 123i64 {
        case x: Int64 => x == 123i64
        case _: _ => false
    }

    @test
    def reifyBigInt01(): Bool = matchType 123ii {
        case _: BigInt => true
        case _: _ => false
    }

    @test
    def reifyBigInt02(): Bool = matchType 123ii {
        case x: BigInt => x == 123ii
        case _: _ => false
    }

    @test
    def reifyString01(): Bool = matchType "hello" {
        case _: String => true
        case _: _ => false
    }

    @test
    def reifyString02(): Bool = matchType "hello" {
        case x: String => x == "hello"
        case _: _ => false
    }

    @test
    def reifyArrayUnit01(): Bool \ IO = matchType [()] {
        case _: Array[Unit, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayUnit02(): Bool \ IO = matchType [()] {
        case x: Array[Unit, Static] => Array.sameElements(x, [()])
        case _: _ => false
    }

    @test
    def reifyArrayBool01(): Bool \ IO = matchType [true] {
        case _: Array[Bool, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayBool02(): Bool \ IO = matchType [true] {
        case x: Array[Bool, Static] => Array.sameElements(x, [true])
        case _: _ => false
    }

    @test
    def reifyArrayChar01(): Bool \ IO = matchType ['a'] {
        case _: Array[Char, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayChar02(): Bool \ IO = matchType ['a'] {
        case x: Array[Char, Static] => Array.sameElements(x, ['a'])
        case _: _ => false
    }

    @test
    def reifyArrayFloat32_01(): Bool \ IO = matchType [0.123f32] {
        case _: Array[Float32, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayFloat32_02(): Bool \ IO = matchType [0.123f32] {
        case x: Array[Float32, Static] => Array.sameElements(x, [0.123f32])
        case _: _ => false
    }

    @test
    def reifyArrayFloat64_01(): Bool \ IO = matchType [0.123f64] {
        case _: Array[Float64, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayFloat64_02(): Bool \ IO = matchType [0.123f64] {
        case x: Array[Float64, Static] => Array.sameElements(x, [0.123f64])
        case _: _ => false
    }

    @test
    def reifyArrayInt8_01(): Bool \ IO = matchType [123i8] {
        case _: Array[Int8, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayInt8_02(): Bool \ IO = matchType [123i8] {
        case x: Array[Int8, Static] => Array.sameElements(x, [123i8])
        case _: _ => false
    }

    @test
    def reifyArrayInt16_01(): Bool \ IO = matchType [123i16] {
        case _: Array[Int16, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayInt16_02(): Bool \ IO = matchType [123i16] {
        case x: Array[Int16, Static] => Array.sameElements(x, [123i16])
        case _: _ => false
    }

    @test
    def reifyArrayInt32_01(): Bool \ IO = matchType [123i32] {
        case _: Array[Int32, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayInt32_02(): Bool \ IO = matchType [123i32] {
        case x: Array[Int32, Static] => Array.sameElements(x, [123i32])
        case _: _ => false
    }

    @test
    def reifyArrayInt64_01(): Bool \ IO = matchType [123i64] {
        case _: Array[Int64, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayInt64_02(): Bool \ IO = matchType [123i64] {
        case x: Array[Int64, Static] => Array.sameElements(x, [123i64])
        case _: _ => false
    }

    @test
    def reifyArrayBigInt01(): Bool \ IO = matchType [123ii] {
        case _: Array[BigInt, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayBigInt02(): Bool \ IO = matchType [123ii] {
        case x: Array[BigInt, Static] => Array.sameElements(x, [123ii])
        case _: _ => false
    }

    @test
    def reifyArrayString01(): Bool \ IO = matchType ["hello"] {
        case _: Array[String, Static] => true
        case _: _ => false
    }

    @test
    def reifyArrayString02(): Bool \ IO = matchType ["hello"] {
        case x: Array[String, Static] => Array.sameElements(x, ["hello"])
        case _: _ => false
    }
}
