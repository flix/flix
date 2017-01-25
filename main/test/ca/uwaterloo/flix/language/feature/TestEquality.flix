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

//
// # Tuples
//

@test
def eqTuple01: Bool = assert!(() == ())

@test
def eqTuple02: Bool = assert!((1, 2) == (1, 2))

@test
def eqTuple03: Bool = assert!((1, 2, 3) == (1, 2, 3))

@test
def eqTuple04: Bool = assert!((1, 2, 3, 4) == (1, 2, 3, 4))

@test
def eqTuple05: Bool = assert!((1, 2, 3, 4, 5) == (1, 2, 3, 4, 5))

@test
def eqTuple06: Bool = assert!((true, "foo") == (true, "foo"))

@test
def eqTuple07: Bool = assert!((true, 'a', "foo") == (true, 'a', "foo"))

@test
def eqTuple08: Bool = assert!((true, 'a', 42, "foo", ()) == (true, 'a', 42, "foo", ()))

@test
def eqTuple09: Bool = assert!((1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64) == (1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64))

@test
def eqTuple10: Bool = assert!((1, (2, 3)) == (1, (2, 3)))

@test
def eqTuple11: Bool = assert!((1, (2, (3, 4))) == (1, (2, (3, 4))))

@test
def eqTuple12: Bool = assert!(((1, 2), (3, 4)) == ((1, 2), (3, 4)))

@test
def eqTuple13: Bool = assert!(((1, 2), (3, (4, 5))) == ((1, 2), (3, (4, 5))))

@test
def neqTuple01: Bool = assert!(true)

@test
def neqTuple02: Bool = assert!((1, 2) != (2, 1))

@test
def neqTuple03: Bool = assert!((1, 2, 3) != (3, 2, 1))

@test
def neqTuple04: Bool = assert!((1, 2, 3, 4) != (1, 2, 4, 3))

@test
def neqTuple05: Bool = assert!((1, 2, 3, 4, 5) != (1, 5, 3, 4, 2))

@test
def neqTuple06: Bool = assert!((true, "foo") != (false, "foo"))

@test
def neqTuple07: Bool = assert!((true, 'a', "foo") != (true, 'b', "foo"))

@test
def neqTuple08: Bool = assert!((true, 'a', 42, "foo", ()) != (true, 'a', 21, "foo", ()))

@test
def neqTuple09: Bool = assert!((1i8, 2i16, 3i32, 4i64, 5.0f32, 6.0f64) != (1i8, 2i16, 3i32, 4i64, 5.0f32, 42.0f64))

@test
def neqTuple10: Bool = assert!((1, (2, 3)) != (1, (3, 2)))

@test
def neqTuple11: Bool = assert!((1, (2, (3, 4))) != (1, (3, (2, 4))))

@test
def neqTuple12: Bool = assert!(((1, 2), (3, 4)) != ((1, 2), (42, 4)))

@test
def neqTuple13: Bool = assert!(((1, 2), (3, (4, 5))) != ((1, 2), (3, (42, 5))))

//
// # Tags
//

@test
def eqTag01: Bool = assert!(None == None)

@test
def eqTag02: Bool = assert!(Some(42) == Some(42))

@test
def eqTag03: Bool = assert!(Ok(42) == Ok(42))

@test
def eqTag04: Bool = assert!(Err(42) == Err(42))

@test
def eqTag05: Bool = assert!(Nil == Nil)

@test
def eqTag06: Bool = assert!((42 :: Nil) == (42 :: Nil))

@test
def eqTag07: Bool = assert!(Some(Ok(42)) == Some(Ok(42)))

@test
def eqTag08: Bool = assert!(Ok(Some(42)) == Ok(Some(42)))

@test
def eqTag09: Bool = assert!(Some((Ok(1) :: Err(2) :: Nil)) == Some((Ok(1) :: Err(2) :: Nil)))

@test
def neqTag01: Bool = assert!(None != Some(42))

@test
def neqTag02: Bool = assert!(Some(21) != Some(42))

@test
def neqTag03: Bool = assert!(Ok(21) != Ok(42))

@test
def neqTag04: Bool = assert!(Err(21) != Err(42))

@test
def neqTag05: Bool = assert!(Nil != (42 :: Nil))

@test
def neqTag06: Bool = assert!((21 :: Nil) != (42 :: Nil))

@test
def neqTag07: Bool = assert!(Some(Ok(21)) != Some(Err(42)))

@test
def neqTag08: Bool = assert!(Ok(None) != Ok(Some(42)))

@test
def neqTag09: Bool = assert!(Some((Ok(1) :: Err(2) :: Nil)) != Some((Err(1) :: Ok(2) :: Nil)))

