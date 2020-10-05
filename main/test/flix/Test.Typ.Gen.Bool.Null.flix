namespace Test/Typ/Gen/Bool/Null {

    pub def testNullableOneVar01(x: Choice[String, false, true]): Bool =
        choose x {
            case Present(y) => y == "Hello World"
        }

    pub def testNullableOneVar02(x: Choice[String, not not n, true]): Bool =
        choose x {
            case _ => true
        }

    pub def testNullableOneVar03(x: Choice[String, true, true]): Bool =
        choose x {
            case _ => true
        }

    pub def testNullableOneVar04(x: Choice[String, false, true]): Bool =
        choose x {
            case _ => true
        }

    pub def testNullableOneVarExhaustive01(x: Choice[String, false, true]): Bool =
        choose x {
            case Present(y) => y == "Hello World"
            case _          => true
        }

    pub def testNullableOneVarExhaustive02(x: Choice[String, true, true]): Bool =
        choose x {
            case Present(y) => y == "Hello World"
            case _          => true
        }

    pub def testNullableOneVarExhaustive03(x: Choice[String, not not n, true]): Bool =
        choose x {
            case Present(y) => y == "Hello World"
            case _          => true
        }

    pub def testNullableTwoVar01(x: Choice[String, false, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (Present(a), Present(b)) => a == "Hello" && b == "World"
        }

    pub def testNullableTwoVar02(x: Choice[String, false, true], y: Choice[String, not not n, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
        }

    pub def testNullableTwoVar03(x: Choice[String, false, true], y: Choice[String, true, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
        }

    pub def testNullableTwoVar04(x: Choice[String, false, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
        }

    pub def testNullableTwoVar05(x: Choice[String, not not n, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar06(x: Choice[String, true, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar07(x: Choice[String, false, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar08(x: Choice[String, false, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar09(x: Choice[String, false, true], y: Choice[String, true, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar10(x: Choice[String, true, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar11(x: Choice[String, not not n, true], y: Choice[String, not n, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar12(x: Choice[String, not n, true], y: Choice[String, not not n, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar13(x: Choice[String, not not n1, true], y: Choice[String, not n1 and n2, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVar14(x: Choice[String, not n1 and n2, true], y: Choice[String, not not n1, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
        }

    pub def testNullableTwoVarExhaustive01(x: Choice[String, false, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
            case (_, _)          => true
        }

    pub def testNullableTwoVarExhaustive02(x: Choice[String, true, true], y: Choice[String, false, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
            case (_, _)          => true
        }

    pub def testNullableTwoVarExhaustive03(x: Choice[String, false, true], y: Choice[String, true, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
            case (_, _)          => true
        }

    pub def testNullableTwoVarExhaustive04(x: Choice[String, true, true], y: Choice[String, true, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
            case (_, _)          => true
        }

    pub def testNullableTwoVarExhaustive05(x: Choice[String, not not n, true], y: Choice[String, not not m, true]): Bool =
        choose (x, y) {
            case (Present(a), _) => a == "Hello"
            case (_, Present(b)) => b == "World"
            case (_, _)          => true
        }

    pub def testNullableThreeVar01(x: Choice[String, false, true], y: Choice[String, false, true], z: Choice[String, false, true]): Bool =
        choose (x, y, z) {
            case (Present(a), Present(b), Present(c)) => a == "Hello" && b == "World" && c == "!"
        }

    pub def testNullableThreeVar02(x: Choice[String, true, true], y: Choice[String, false, true], z: Choice[String, false, true]): Bool =
        choose (x, y, z) {
            case (_, Present(b), Present(c)) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar03(x: Choice[String, false, true], y: Choice[String, true, true], z: Choice[String, false, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, Present(c)) => a == "Hello" && c == "!"
        }

    pub def testNullableThreeVar04(x: Choice[String, false, true], y: Choice[String, false, true], z: Choice[String, true, true]): Bool =
        choose (x, y, z) {
            case (Present(a), Present(b), _) => a == "Hello" && b == "World"
        }

    pub def testNullableThreeVar05(x: Choice[String, false, true], y: Choice[String, true, true], z: Choice[String, true, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
        }

    pub def testNullableThreeVar06(x: Choice[String, true, true], y: Choice[String, false, true], z: Choice[String, true, true]): Bool =
        choose (x, y, z) {
            case (_, Present(b), _) => b == "World"
        }

    pub def testNullableThreeVar07(x: Choice[String, true, true], y: Choice[String, true, true], z: Choice[String, false, true]): Bool =
        choose (x, y, z) {
            case (_, _, Present(c)) => c == "!"
        }

    pub def testNullableThreeVar08(x: Choice[String, false, true], y: Choice[String, false, true], z: Choice[String, false, true]): Bool =
        choose (x, y, z) {
            case (Present(a), Present(b), _) => a == "Hello" && b == "World"
            case (_, Present(b), Present(c)) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar09(x: Choice[String, true, true], y: Choice[String, false, true], z: Choice[String, false, true]): Bool =
        choose (x, y, z) {
            case (Present(a), Present(b), _) => a == "Hello" && b == "World"
            case (_, Present(b), Present(c)) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar10(x: Choice[String, false, true], y: Choice[String, false, true], z: Choice[String, true, true]): Bool =
        choose (x, y, z) {
            case (Present(a), Present(b), _) => a == "Hello" && b == "World"
            case (_, Present(b), Present(c)) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar11(x: Choice[String, not not n, true], y: Choice[String, false, true], z: Choice[String, not n, true]): Bool =
        choose (x, y, z) {
            case (Present(a), Present(b), _) => a == "Hello" && b == "World"
            case (_, Present(b), Present(c)) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar12(x: Choice[String, not n, true], y: Choice[String, false, true], z: Choice[String, not not n, true]): Bool =
        choose (x, y, z) {
            case (Present(a), Present(b), _) => a == "Hello" && b == "World"
            case (_, Present(b), Present(c)) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar13(x: Choice[String, false, true], y: Choice[String, false, true], z: Choice[String, false, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
            case (_, Present(b), _) => b == "World"
            case (_, _, Present(c)) => c == "!"
        }

    pub def testNullableThreeVar14(x: Choice[String, true, true], y: Choice[String, true, true], z: Choice[String, false, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
            case (_, Present(b), _) => b == "World"
            case (_, _, Present(c)) => c == "!"
        }

    pub def testNullableThreeVar15(x: Choice[String, true, true], y: Choice[String, false, true], z: Choice[String, true, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
            case (_, Present(b), _) => b == "World"
            case (_, _, Present(c)) => c == "!"
        }

    pub def testNullableThreeVar16(x: Choice[String, false, true], y: Choice[String, false, true], z: Choice[String, true, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
            case (_, Present(b), _) => b == "World"
            case (_, _, Present(c)) => c == "!"
        }

    pub def testNullableThreeVar17(x: Choice[String, not not n, true], y: Choice[String, not not n, true], z: Choice[String, not n, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
            case (_, Present(b), _) => b == "World"
            case (_, _, Present(c)) => c == "!"
        }

    pub def testNullableThreeVar18(x: Choice[String, not not n, true], y: Choice[String, not n, true], z: Choice[String, not n, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
            case (_, Present(b), _) => b == "World"
            case (_, _, Present(c)) => c == "!"
        }

    pub def testNullableThreeVar19(x: Choice[String, not n, true], y: Choice[String, not not n, true], z: Choice[String, not n, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
            case (_, Present(b), _) => b == "World"
            case (_, _, Present(c)) => c == "!"
        }

    pub def testNullableThreeVar20(x: Choice[String, not n, true], y: Choice[String, not n, true], z: Choice[String, not not n, true]): Bool =
        choose (x, y, z) {
            case (Present(a), _, _) => a == "Hello"
            case (_, Present(b), _) => b == "World"
            case (_, _, Present(c)) => c == "!"
        }

    // TODO: Fix use of "not not".

}
