@test
def testAssign01(): Bool =
    let l = ref 42;
    let _ = l := 84;
    84 `assertEq!` deref l

@test
def testAssign02(): Bool =
    let l = ref (1, 2, 3);
    let _ = l := (3, 2, 1);
    (3, 2, 1) `assertEq!` deref l

@test
def testAssign03(): Bool =
    let l = ref None : Option[Int];
    let _ = l := Some(42);
    Some(42) `assertEq!` deref l

@test
def testAssign04(): Bool =
    let l = ref Some(42);
    let _ = l := None : Option[Int];
    None `assertEq!` deref l
