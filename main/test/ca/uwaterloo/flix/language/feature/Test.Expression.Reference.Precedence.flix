@test
def testRefPrecedence01(): Bool =
    let l = ref ref 123;
    let r = deref deref l;
    123 `assertEq!` r
