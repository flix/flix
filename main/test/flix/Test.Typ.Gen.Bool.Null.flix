namespace Test/Typ/Gen/Bool/Null {

    pub def testNullableOneVar01(x: Nullable[String, false]): Bool =
        match? x {
            case y => y == "Hello World"
        }

    pub def testNullableOneVar02(x: Nullable[String, n]): Bool =
        match? x {
            case _ => true
        }

    pub def testNullableOneVar03(x: Nullable[String, true]): Bool =
        match? x {
            case _ => true
        }

    pub def testNullableOneVar04(x: Nullable[String, false]): Bool =
        match? x {
            case _ => true
        }

    pub def testNullableOneVarExhaustive01(x: Nullable[String, false]): Bool =
        match? x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableOneVarExhaustive02(x: Nullable[String, true]): Bool =
        match? x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableOneVarExhaustive03(x: Nullable[String, n]): Bool =
        match? x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableTwoVar01(x: Nullable[String, false], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (a, b) => a == "Hello" && b == "World"
        }

    pub def testNullableTwoVar02(x: Nullable[String, false], y: Nullable[String, n]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar03(x: Nullable[String, false], y: Nullable[String, true]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar04(x: Nullable[String, false], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableTwoVar05(x: Nullable[String, n], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar06(x: Nullable[String, true], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar07(x: Nullable[String, false], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar08(x: Nullable[String, false], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar09(x: Nullable[String, false], y: Nullable[String, true]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar10(x: Nullable[String, true], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar11(x: Nullable[String, n], y: Nullable[String, not n]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar12(x: Nullable[String, not n], y: Nullable[String, n]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar13(x: Nullable[String, n1], y: Nullable[String, not n1 or n2]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVar14(x: Nullable[String, not n1 or n2], y: Nullable[String, n1]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableTwoVarExhaustive01(x: Nullable[String, false], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive02(x: Nullable[String, true], y: Nullable[String, false]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive03(x: Nullable[String, false], y: Nullable[String, true]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive04(x: Nullable[String, true], y: Nullable[String, true]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableTwoVarExhaustive05(x: Nullable[String, n], y: Nullable[String, m]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
            case (_, _) => true
        }

    pub def testNullableThreeVar01(x: Nullable[String, false], y: Nullable[String, false], z: Nullable[String, false]): Bool =
        match? (x, y, z) {
            case (a, b, c) => a == "Hello" && b == "World" && c == "!"
        }

    pub def testNullableThreeVar02(x: Nullable[String, true], y: Nullable[String, false], z: Nullable[String, false]): Bool =
        match? (x, y, z) {
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar03(x: Nullable[String, false], y: Nullable[String, true], z: Nullable[String, false]): Bool =
        match? (x, y, z) {
            case (a, _, c) => a == "Hello" && c == "!"
        }

    pub def testNullableThreeVar04(x: Nullable[String, false], y: Nullable[String, false], z: Nullable[String, true]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
        }

    pub def testNullableThreeVar05(x: Nullable[String, false], y: Nullable[String, true], z: Nullable[String, true]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
        }

    pub def testNullableThreeVar06(x: Nullable[String, true], y: Nullable[String, false], z: Nullable[String, true]): Bool =
        match? (x, y, z) {
            case (_, b, _) => b == "World"
        }

    pub def testNullableThreeVar07(x: Nullable[String, true], y: Nullable[String, true], z: Nullable[String, false]): Bool =
        match? (x, y, z) {
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar08(x: Nullable[String, false], y: Nullable[String, false], z: Nullable[String, false]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar09(x: Nullable[String, true], y: Nullable[String, false], z: Nullable[String, false]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar10(x: Nullable[String, false], y: Nullable[String, false], z: Nullable[String, true]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar11(x: Nullable[String, n], y: Nullable[String, false], z: Nullable[String, not n]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar12(x: Nullable[String, not n], y: Nullable[String, false], z: Nullable[String, n]): Bool =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testNullableThreeVar13(x: Nullable[String, false], y: Nullable[String, false], z: Nullable[String, false]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar14(x: Nullable[String, true], y: Nullable[String, true], z: Nullable[String, false]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar15(x: Nullable[String, true], y: Nullable[String, false], z: Nullable[String, true]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar16(x: Nullable[String, false], y: Nullable[String, false], z: Nullable[String, true]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar17(x: Nullable[String, n], y: Nullable[String, n], z: Nullable[String, not n]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar18(x: Nullable[String, n], y: Nullable[String, not n], z: Nullable[String, not n]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar19(x: Nullable[String, not n], y: Nullable[String, n], z: Nullable[String, not n]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testNullableThreeVar20(x: Nullable[String, not n], y: Nullable[String, not n], z: Nullable[String, n]): Bool =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

}
