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

}
