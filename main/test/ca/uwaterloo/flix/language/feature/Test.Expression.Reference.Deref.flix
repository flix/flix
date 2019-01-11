@test
def testDeref01(): Bool =
    let l = ref ();
    () `assertEq!` deref l

@test
def testDeref02(): Bool =
    let l = ref true;
    true `assertEq!` deref l

@test
def testDeref03(): Bool =
    let l = ref false;
    false `assertEq!` deref l

@test
def testDeref04(): Bool =
    let l = ref 'a';
    'a' `assertEq!` deref l

@test
def testDeref05(): Bool =
    let l = ref 0.0f32;
    0.0f32 `assertEq!` deref l

@test
def testDeref06(): Bool =
    let l = ref 0.0f64;
    0.0f64 `assertEq!` deref l

@test
def testDeref07(): Bool =
    let l = ref 0i8;
    0i8 `assertEq!` deref l

@test
def testDeref08(): Bool =
    let l = ref 0i16;
    0i16 `assertEq!` deref l

@test
def testDeref09(): Bool =
    let l = ref 0i32;
    0i32 `assertEq!` deref l

@test
def testDeref10(): Bool =
    let l = ref 0i64;
    0i64 `assertEq!` deref l

@test
def testDeref11(): Bool =
    let l = ref 0ii;
    0ii `assertEq!` deref l

@test
def testDeref12(): Bool =
    let l = ref "hello world";
    "hello world" `assertEq!` deref l

@test
def testDeref13(): Bool =
    let l = ref (1, 2, 3);
    (1, 2, 3) `assertEq!` deref l

@test
def testDeref14(): Bool =
    let l = ref None : Option[Int];
    None `assertEq!` deref l

@test
def testDeref15(): Bool =
    let l = ref Some(42);
    Some(42) `assertEq!` deref l
