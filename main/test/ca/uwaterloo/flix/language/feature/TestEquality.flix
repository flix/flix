//
// # Unit
//
@test
def eqUnit: Bool = assert!(() == ())

//
// # Bool
//

@test
def eqBool01: Bool = assert!(true == true)

@test
def eqBool02: Bool = assert!(false == false)

@test
def neqBool01: Bool = assert!(true != false)

@test
def neqBool02: Bool = assert!(false != true)

//
// # Char
//

@test
def eqChar01: Bool = assert!('a' == 'a')

@test
def eqChar02: Bool = assert!('z' == 'z')

@test
def neqChar01: Bool = assert!('a' != 'b')

@test
def neqChar02: Bool = assert!('z' != 'w')

//
// # Float32
//

@test
def eqFloat32_01: Bool = assert!(+0.0f32 == +0.0f32)

@test
def eqFloat32_02: Bool = assert!(-0.0f32 == -0.0f32)

@test
def eqFloat32_03: Bool = assert!(+1.0f32 == +1.0f32)

@test
def eqFloat32_04: Bool = assert!(-1.0f32 == -1.0f32)

@test
def neqFloat32_01: Bool = assert!(-1.0f32 != +1.0f32)

@test
def neqFloat32_02: Bool = assert!(+1.0f32 != -1.0f32)

//
// # Float64
//

@test
def eqFloat64_01: Bool = assert!(+0.0f64 == +0.0f64)

@test
def eqFloat64_02: Bool = assert!(-0.0f64 == -0.0f64)

@test
def eqFloat64_03: Bool = assert!(+1.0f64 == +1.0f64)

@test
def eqFloat64_04: Bool = assert!(-1.0f64 == -1.0f64)

@test
def neqFloat64_01: Bool = assert!(-1.0f64 != +1.0f64)

@test
def neqFloat64_02: Bool = assert!(+1.0f64 != -1.0f64)

//
// # Int8
//

@test
def eqInt8_01: Bool = assert!(0i8 == 0i8)

@test
def eqInt8_02: Bool = assert!(+1i8 == +1i8)

@test
def eqInt8_03: Bool = assert!(-1i8 == -1i8)

@test
def neqInt8_01: Bool = assert!(-1i8 != +1i8)

@test
def neqInt8_02: Bool = assert!(+1i8 != -1i8)


//
// # Int16
//

@test
def eqInt16_01: Bool = assert!(0i16 == 0i16)

@test
def eqInt16_02: Bool = assert!(+1i16 == +1i16)

@test
def eqInt16_03: Bool = assert!(-1i16 == -1i16)

@test
def neqInt16_01: Bool = assert!(-1i16 != +1i16)

@test
def neqInt16_02: Bool = assert!(+1i16 != -1i16)

//
// # Int32
//

@test
def eqInt32_01: Bool = assert!(0i32 == 0i32)

@test
def eqInt32_02: Bool = assert!(+1i32 == +1i32)

@test
def eqInt32_03: Bool = assert!(-1i32 == -1i32)

@test
def neqInt32_01: Bool = assert!(-1i32 != +1i32)

@test
def neqInt32_02: Bool = assert!(+1i32 != -1i32)

//
// # Int64
//

@test
def eqInt64_01: Bool = assert!(0i64 == 0i64)

@test
def eqInt64_02: Bool = assert!(+1i64 == +1i64)

@test
def eqInt64_03: Bool = assert!(-1i64 == -1i64)

@test
def neqInt64_01: Bool = assert!(-1i64 != +1i64)

@test
def neqInt64_02: Bool = assert!(+1i64 != -1i64)

//
// # BigInt
//

@test
def eqBigInt01: Bool = assert!(0ii == 0ii)

@test
def eqBigInt02: Bool = assert!(+1ii == +1ii)

@test
def eqBigInt03: Bool = assert!(-1ii == -1ii)

@test
def neqBigInt01: Bool = assert!(-1ii != +1ii)

@test
def neqBigInt02: Bool = assert!(+1ii != -1ii)

//
// # Str
//

@test
def eqStr01: Bool = assert!("" == "")

@test
def eqStr02: Bool = assert!("a" == "a")

@test
def eqStr03: Bool = assert!("abc" == "abc")

@test
def neqStr01: Bool = assert!("" != "a")

@test
def neqStr02: Bool = assert!("a" != "abc")

