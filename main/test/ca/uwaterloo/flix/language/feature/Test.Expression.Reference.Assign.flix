@test
def testAssignUnit01(): Bool =
    let l = ref ();
    l := ();
    () `assertEq!` deref l

@test
def testAssignBool01(): Bool =
    let l = ref true;
    l := false;
    false `assertEq!` deref l

@test
def testAssignBool02(): Bool =
    let l = ref false;
    l := true;
    true `assertEq!` deref l

@test
def testAssignChar01(): Bool =
    let l = ref 'a';
    l := 'b';
    'b' `assertEq!` deref l

@test
def testAssignFloat3201(): Bool =
    let l = ref 123.456f32;
    l := 456.123f32;
    456.123f32 `assertEq!` deref l

@test
def testAssignFloat6401(): Bool =
    let l = ref 123.456f64;
    l := 456.123f64;
    456.123f64 `assertEq!` deref l

@test
def testAssignInt801(): Bool =
    let l = ref 123i8;
    l := 111i8;
    111i8 `assertEq!` deref l

@test
def testAssignInt1601(): Bool =
    let l = ref 123i16;
    l := 456i16;
    456i16 `assertEq!` deref l

@test
def testAssignInt3201(): Bool =
    let l = ref 123i32;
    l := 456i32;
    456i32 `assertEq!` deref l

@test
def testAssignInt6401(): Bool =
    let l = ref 123i64;
    l := 456i64;
    456i64 `assertEq!` deref l

@test
def testAssignBigInt01(): Bool =
    let l = ref 111111111ii;
    l := 999999999ii;
    999999999ii `assertEq!` deref l

@test
def testAssignStr01(): Bool =
    let l = ref "hello";
    l := "hello world";
    "hello world" `assertEq!` deref l

@test
def testAssignOption01(): Bool =
    let l = ref None : Option[Int];
    l := Some(123);
    Some(123) `assertEq!` deref l

@test
def testAssignOption02(): Bool =
    let l = ref Some(123);
    l := None;
    None `assertEq!` deref l

@test
def testAssignResult01(): Bool =
    let l = ref Ok(123);
    l := Err(456);
    Err(456) `assertEq!` deref l

@test
def testAssignResult02(): Bool =
    let l = ref Err(123);
    l := Ok(456);
    Ok(456) `assertEq!` deref l

@test
def testAssignAssign01(): Bool =
    let l = ref 1;
    l := 2;
    l := 3;
    3 `assertEq!` deref l

@test
def testAssignAssign02(): Bool =
    let l = ref 1;
    l := 2;
    l := 3;
    l := 4;
    4 `assertEq!` deref l

@test
def testAssignAssign03(): Bool =
    let l = ref 1;
    l := 2;
    l := 3;
    l := 4;
    l := 5;
    5 `assertEq!` deref l

@test
def testAssignAssign04(): Bool =
    let l = ref 1;
    l := deref l + 1;
    2 `assertEq!` deref l

@test
def testAssignAssign05(): Bool =
    let l = ref 1;
    l := deref l + 1;
    l := deref l + 1;
    3 `assertEq!` deref l

@test
def testAssignAssign06(): Bool =
    let l = ref 1;
    l := deref l + 1;
    l := deref l + 1;
    l := deref l + 1;
    4 `assertEq!` deref l

@test
def testAssignAssign07(): Bool =
    let l = ref 1;
    l := deref l + 1;
    l := deref l + 1;
    l := deref l + 1;
    l := deref l + 1;
    5 `assertEq!` deref l

@test
def testRefRef01(): Bool =
    let l = ref (ref 123);
    (deref l) := 456;
    456 `assertEq!` deref (deref l)

@test
def testRefRef02(): Bool =
    let l = ref (ref (ref 123));
    (deref (deref l)) := 456;
    456 `assertEq!` deref (deref (deref l))

@test
def testAlias01(): Bool =
    let l1 = ref (ref 123);
    let l2 = ref 456;
    l1 := l2;
    456 `assertEq!` deref (deref l1)

@test
def testAlias02(): Bool =
    let l1 = ref (ref 123);
    let l2 = ref 456;
    l1 := l2;
    l2 := 789;
    789 `assertEq!` deref (deref l1)
