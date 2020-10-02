namespace Test/Typ/Gen/Bool/Null {

    pub def testNullableOneVar01(x: String ? false): Bool =
        choice x {
            case y => y == "Hello World"
        }

    pub def testNullableOneVar02(x: String ? n): Bool =
        choice x {
            case _ => true
        }

    pub def testNullableOneVar03(x: String ? true): Bool =
        choice x {
            case _ => true
        }

    pub def testNullableOneVar04(x: String ? false): Bool =
        choice x {
            case _ => true
        }

    pub def testNullableOneVarExhaustive01(x: String ? false): Bool =
        choice x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableOneVarExhaustive02(x: String ? true): Bool =
        choice x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableOneVarExhaustive03(x: String ? n): Bool =
        choice x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableTwoVar01(x: String ? false, y: String ? false): Bool =
        choice (x, y) {
            case (a, b) => a == "Hello" && b == "World"
        }

    pub def testNullableTwoVar02(x: String ? false, y: String ? n): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar03(x: String ? false, y: String ? true): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar04(x: String ? false, y: String ? false): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar05(x: String ? n, y: String ? false): Bool =
        choice (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar06(x: String ? true, y: String ? false): Bool =
        choice (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar07(x: String ? false, y: String ? false): Bool =
        choice (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar08(x: String ? false, y: String ? false): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar09(x: String ? false, y: String ? true): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar10(x: String ? true, y: String ? false): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar11(x: String ? n, y: String ? (not n)): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar12(x: String ? (not n), y: String ? n): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar13(x: String ? n1, y: String ? (not n1 and n2)): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar14(x: String ? (not n1 and n2), y: String ? n1): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVarExhaustive01(x: String ? false, y: String ? false): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive02(x: String ? true, y: String ? false): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive03(x: String ? false, y: String ? true): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive04(x: String ? true, y: String ? true): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive05(x: String ? n, y: String ? m): Bool =
        choice (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableThreeVar01(x: String ? false, y: String ? false, z: String ? false): Bool =
        choice (x, y, z) {
            case (a, b, c) => a == "Hello" && b == "World" && c == "!"
        }

    pub def testNullableThreeVar02(x: String ? true, y: String ? false, z: String ? false): Bool =
        choice (x, y, z) {
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar03(x: String ? false, y: String ? true, z: String ? false): Bool =
        choice (x, y, z) {
            case (a, _, c) => a == "Hello" && c == "!"
        }

    pub def testNullableThreeVar04(x: String ? false, y: String ? false, z: String ? true): Bool =
        choice (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
        }

    pub def testNullableThreeVar05(x: String ? false, y: String ? true, z: String ? true): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
        }

    pub def testNullableThreeVar06(x: String ? true, y: String ? false, z: String ? true): Bool =
        choice (x, y, z) {
            case (_, b, _) => b == "World"
        }

    pub def testNullableThreeVar07(x: String ? true, y: String ? true, z: String ? false): Bool =
        choice (x, y, z) {
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar08(x: String ? false, y: String ? false, z: String ? false): Bool =
        choice (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar09(x: String ? true, y: String ? false, z: String ? false): Bool =
        choice (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar10(x: String ? false, y: String ? false, z: String ? true): Bool =
        choice (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar11(x: String ? n, y: String ? false, z: String ? (not n)): Bool =
        choice (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar12(x: String ? (not n), y: String ? false, z: String ? n): Bool =
        choice (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar13(x: String ? false, y: String ? false, z: String ? false): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar14(x: String ? true, y: String ? true, z: String ? false): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar15(x: String ? true, y: String ? false, z: String ? true): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar16(x: String ? false, y: String ? false, z: String ? true): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar17(x: String ? n, y: String ? n, z: String ? (not n)): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar18(x: String ? n, y: String ? (not n), z: String ? (not n)): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar19(x: String ? (not n), y: String ? n, z: String ? (not n)): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar20(x: String ? (not n), y: String ? (not n), z: String ? n): Bool =
        choice (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

}
