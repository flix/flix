namespace Test/Typ/Gen/Bool/Null {

    pub def testNullableOneVar01(x: Null[false, String]): Bool =
        match? x {
            case y => y == "Hello World"
        }

    pub def testNullableOneVar02(x: Null[n, String]): Bool =
        match? x {
            case _ => true
        }

    pub def testNullableOneVar03(x: Null[true, String]): Bool =
        match? x {
            case _ => true
        }

    pub def testNullableOneVar04(x: Null[false, String]): Bool =
        match? x {
            case _ => true
        }

    pub def testNullableOneVarExhaustive01(x: Null[false, String]): Bool =
        match? x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableOneVarExhaustive02(x: Null[true, String]): Bool =
        match? x {
            case y => y == "Hello World"
            case _ => true
        }

    // TODO
//    pub def testNullableOneVarExhaustive03(x: Null[n, String]): Bool =
//        match? x {
//            case y => y == "Hello World"
//            case _ => true
//        }

    pub def testNullableTwoVar01(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, b) => a == "Hello" && b == "World"
        }

    pub def testNullableTwoVar02(x: Null[false, String], y: Null[n, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar03(x: Null[false, String], y: Null[true, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar04(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar05(x: Null[n, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar06(x: Null[true, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar07(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar08(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar09(x: Null[false, String], y: Null[true, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar10(x: Null[true, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    // TODO: More polymorphic cases.

    pub def testNullableTwoVarExhaustive01(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive02(x: Null[true, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive03(x: Null[false, String], y: Null[true, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive04(x: Null[true, String], y: Null[true, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    // TODO
//    pub def testNullableTwoVarExhaustive05(x: Null[n, String], y: Null[m, String]): Bool =
//        match? (x, y) {
//            case (a, _) => a == "Hello"
//            case (_, b) => b == "World"
//            case (_, _) => true
//        }

    pub def testNullableThreeVar01(x: Null[false, String], y: Null[false, String], z: Null[false, String]): Bool =
        match? (x, y, z) {
            case (a, b, c) => a == "Hello" && b == "World" && c == "!"
        }

    pub def testNullableThreeVar02(x: Null[true, String], y: Null[false, String], z: Null[false, String]): Bool =
        match? (x, y, z) {
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar03(x: Null[false, String], y: Null[true, String], z: Null[false, String]): Bool =
        match? (x, y, z) {
            case (a, _, c) => a == "Hello" && c == "!"
        }

    pub def testNullableThreeVar04(x: Null[false, String], y: Null[false, String], z: Null[true, String]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
        }

    pub def testNullableThreeVar05(x: Null[false, String], y: Null[true, String], z: Null[true, String]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
        }

    pub def testNullableThreeVar06(x: Null[true, String], y: Null[false, String], z: Null[true, String]): Bool =
        match? (x, y, z) {
            case (_, b, _) => b == "World"
        }

    pub def testNullableThreeVar07(x: Null[true, String], y: Null[true, String], z: Null[false, String]): Bool =
        match? (x, y, z) {
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar08(x: Null[false, String], y: Null[false, String], z: Null[false, String]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar09(x: Null[true, String], y: Null[false, String], z: Null[false, String]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar10(x: Null[false, String], y: Null[false, String], z: Null[true, String]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    // TODO
//    pub def testNullableThreeVar11(x: Null[n, String], y: Null[false, String], z: Null[not n, String]): Bool =
//        match? (x, y, z) {
//            case (a, b, _) => a == "Hello" && b == "World"
//            case (_, b, c) => b == "World" && c == "!"
//        }

    pub def testNullableThreeVar12(x: Null[false, String], y: Null[false, String], z: Null[false, String]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar13(x: Null[true, String], y: Null[true, String], z: Null[false, String]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar14(x: Null[true, String], y: Null[false, String], z: Null[true, String]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar15(x: Null[false, String], y: Null[false, String], z: Null[true, String]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

}
