@test
def curry01(): Bool = f(1, 2) `assertEq!` f(1)(2)

@test
def curry02(): Bool = g(1, 2, 3) `assertEq!` g(1)(2)(3)

@test
def curry03(): Bool = g(1, 2, 3) `assertEq!` g(1, 2)(3)

@test
def curry04(): Bool = g(1, 2, 3) `assertEq!` g(1)(2, 3)

@test
def curry05(): Bool = h(1, 2, 3, 4) `assertEq!` h(1)(2)(3)(4)

@test
def curry06(): Bool = h(1, 2, 3, 4) `assertEq!` h(1, 2)(3)(4)

@test
def curry07(): Bool = h(1, 2, 3, 4) `assertEq!` h(1)(2, 3)(4)

@test
def curry08(): Bool = h(1, 2, 3, 4) `assertEq!` h(1)(2)(3, 4)

@test
def curry09(): Bool = h(1, 2, 3, 4) `assertEq!` h(1, 2)(3, 4)

@test
def curry10(): Bool = h(1, 2, 3, 4) `assertEq!` h(1, 2, 3)(4)

@test
def curry11(): Bool = h(1, 2, 3, 4) `assertEq!` h(1)(2, 3, 4)

@test
def curry12(): Bool = i(1, 2) `assertEq!` i(1)(2)

@test
def curry13(): Bool = j(1, 2, 3) `assertEq!` j(1)(2)(3)

@test
def curry14(): Bool = j(1, 2, 3) `assertEq!` j(1, 2)(3)

@test
def curry15(): Bool = j(1, 2, 3) `assertEq!` j(1)(2, 3)

@test
def curry16(): Bool = k(x -> x, 42) `assertEq!` k(x -> x)(42)

@test
def curry17(): Bool = Option.map(x -> x + 1)(Some(42)) `assertEq!` Some(43)

@test
def curry18(): Bool = Result.map(x -> x + 1)(Ok(42)) `assertEq!` Ok(43)

@test
def curry19(): Bool = List.map(x -> x + 1)(1 :: 2 :: 3 :: Nil) `assertEq!` (2 :: 3 :: 4 :: Nil)

@test
def curry20(): Bool = List.exists(x -> x == 1)(1 :: 2 :: 3 :: Nil) `assertEq!` true

@test
def curry21(): Bool = flip(Option.map)(Some(42), x -> x + 1) `assertEq!` Some(43)

@test
def curry22(): Bool = flip(Option.map)(Some(42))(x -> x + 1) `assertEq!` Some(43)

///
/// A function of arity 2.
///
def f(x: Int, y: Int): Int = x + y

///
/// A function of arity 3.
///
def g(x: Int, y: Int, z: Int): Int = x + y + z

///
/// A function of arity 4.
///
def h(x: Int, y: Int, z: Int, w: Int): Int = x + y + z + w

///
/// A curried function of arity 2.
///
def i(x: Int): Int -> Int = y -> x + y

///
/// A curried function of arity 3.
///
def j(x: Int): Int -> Int -> Int = y -> z -> x + y + z

///
/// A curried higher-order function.
///
def k(f: Int -> Int): Int -> Int = x -> f(x)
