namespace Test/Typ/Gen/Bool/Null {

    pub def testNullableVar01(x: Null[false, String]): Bool =
        match? x {
            case y => y == "Hello World"
        }

    pub def testNullableWild01(x: Null[n, String]): Bool =
        match? x {
            case _ => true
        }

    pub def testNullableWild02(x: Null[true, String]): Bool =
        match? x {
            case _ => true
        }

    pub def testNullableWild03(x: Null[false, String]): Bool =
        match? x {
            case _ => true
        }

//    pub def testNullableExhaustive01(x: Null[n, String]): Bool =
//        match? x {
//            case y => y == "Hello World"
//            case _ => true
//        }

    pub def testNullableExhaustive02(x: Null[true, String]): Bool =
        match? x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableExhaustive03(x: Null[false, String]): Bool =
        match? x {
            case y => y == "Hello World"
            case _ => true
        }

    pub def testNullableMultiVar01(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, b) => a == "Hello" && b == "World"
        }

    pub def testNullableMultiVar02(x: Null[false, String], y: Null[n, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableMultiVar03(x: Null[false, String], y: Null[true, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableMultiVar04(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
        }

    pub def testNullableMultiVar05(x: Null[n, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableMultiVar06(x: Null[true, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableMultiVar07(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (_, b) => b == "World"
        }

    pub def testNullableMultiVar08(x: Null[false, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableMultiVar09(x: Null[false, String], y: Null[true, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testNullableMultiVar10(x: Null[true, String], y: Null[false, String]): Bool =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    // TODO: More polymorphic cases.

}
