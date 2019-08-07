rel R(x: Result[Int, Str])

@test
def testTermLitResult01(): #{ R } =
    let x = R(Ok(123)).;
    solve x

@test
def testTermLitResult02(): #{ R } =
    let x = R(Err("hello world!")).;
    solve x

@test
def testTermLitResult03(): #{ R } =
    let x = R(Ok(123)).;
    let y = R(Err("hello world!")).;
    solve x <+> y

@test
def testTermLitResult04(): #{ R } =
    let x = R(Ok(123)).;
    let y = R(Ok(456)).;
    let z = R(Err("hello world!")).;
    solve x <+> y <+> z

@test
def testTermLitResult05(): #{ R } =
    let x = R(Ok(123)).;
    let y = R(Err("hello world!")).;
    let z = R(Err("Goodbye world!")).;
    solve x <+> y <+> z

@test
def testTermLitResult06(): #{ R } =
    let x = R(Ok(123)).;
    let y = R(Ok(456)).;
    let z = R(Err("hello world!")).;
    let w = R(Err("Goodbye world!")).;
    solve x <+> y <+> z <+> w
