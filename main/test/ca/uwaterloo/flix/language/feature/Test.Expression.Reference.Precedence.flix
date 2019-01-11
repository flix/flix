@test
def testRefPrecedence01(): Bool =
    let l = ref ref 123;
    let r = deref deref l;
    123 `assertEq!` r

@test
def testRefPrecedence02(): Bool =
    let l = ref ref ref 123;
    let r = deref deref deref l;
    123 `assertEq!` r

@test
def testRefPrecedence03(): Bool =
    let x = ref 123;
    let y = ref 123;
    let r = deref x + deref y;
    (123 + 123) `assertEq!` r
