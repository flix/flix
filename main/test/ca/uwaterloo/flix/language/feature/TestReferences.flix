@test
def ref01(): Ref[Unit] = ref ()

@test
def ref02(): Ref[Bool] = ref true

@test
def ref03(): Ref[Bool] = ref false

@test
def ref04(): Ref[Char] = ref 'a'

@test
def ref05(): Ref[Float32] = ref 0.0f32

@test
def ref06(): Ref[Float64] = ref 0.0f64

@test
def ref07(): Ref[Int8] = ref 0i8

@test
def ref08(): Ref[Int16] = ref 0i16

@test
def ref09(): Ref[Int32] = ref 0i32

@test
def ref10(): Ref[Int64] = ref 0i64

@test
def ref11(): Ref[BigInt] = ref 0ii

@test
def ref12(): Ref[Str] = ref "hello world"

@test
def ref13(): Ref[(Int, Int, Int)] = ref (1, 2, 3)

@test
def ref14(): Ref[Option[Int]] = ref None : Option[Int]

@test
def ref15(): Ref[Option[Int]] = ref Some(42)

@test
def deref01(): Bool =
    let l = ref ();
    () `assertEq!` deref l

@test
def deref02(): Bool =
    let l = ref true;
    true `assertEq!` deref l

@test
def deref03(): Bool =
    let l = ref false;
    false `assertEq!` deref l

@test
def deref04(): Bool =
    let l = ref 'a';
    'a' `assertEq!` deref l

@test
def deref05(): Bool =
    let l = ref 0.0f32;
    0.0f32 `assertEq!` deref l

@test
def deref06(): Bool =
    let l = ref 0.0f64;
    0.0f64 `assertEq!` deref l

@test
def deref07(): Bool =
    let l = ref 0i8;
    0i8 `assertEq!` deref l

@test
def deref08(): Bool =
    let l = ref 0i16;
    0i16 `assertEq!` deref l

@test
def deref09(): Bool =
    let l = ref 0i32;
    0i32 `assertEq!` deref l

@test
def deref10(): Bool =
    let l = ref 0i64;
    0i64 `assertEq!` deref l

@test
def deref11(): Bool =
    let l = ref 0ii;
    0ii `assertEq!` deref l

@test
def deref12(): Bool =
    let l = ref "hello world";
    "hello world" `assertEq!` deref l

@test
def deref13(): Bool =
    let l = ref (1, 2, 3);
    (1, 2, 3) `assertEq!` deref l

@test
def deref14(): Bool =
    let l = ref None : Option[Int];
    None `assertEq!` deref l

@test
def deref15(): Bool =
    let l = ref Some(42);
    Some(42) `assertEq!` deref l

@test
def assign01(): Bool =
    let l = ref 42;
    let _ = l := 84;
    84 `assertEq!` deref l

@test
def assign02(): Bool =
    let l = ref (1, 2, 3);
    let _ = l := (3, 2, 1);
    (3, 2, 1) `assertEq!` deref l

@test
def assign03(): Bool =
    let l = ref None : Option[Int];
    let _ = l := Some(42);
    Some(42) `assertEq!` deref l

@test
def assign04(): Bool =
    let l = ref Some(42);
    let _ = l := None : Option[Int];
    None `assertEq!` deref l
