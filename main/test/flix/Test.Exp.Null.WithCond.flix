namespace Test/Exp/Null/WithCond {

    pub def testWithCond01(x: String ? n1, y: String ? n2): Bool with (not (n1 and n2)) =
        match? (x, y) {
            case (a, _) => a == "Hello"
            case (_, b) => b == "World"
        }

    pub def testWithCond02(x: String ? n1, y: String ? false, z: String ? n2): Bool with (not (n1 and n2)) =
        match? (x, y, z) {
            case (a, b, _) => a == "Hello" && b == "World"
            case (_, b, c) => b == "World" && c == "!"
        }

    pub def testWithCond03(x: String ? n1, y: String ? n2, z: String ? n3): Bool with (not (n1 and n2 and n3)) =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testWithCond04(x: String ? n1, y: String ? n2, z: String ? n3): Bool with (not (n1 and n2)) =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testWithCond05(x: String ? n1, y: String ? n2, z: String ? n3): Bool with (not (n1 and n3)) =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

    pub def testWithCond06(x: String ? n1, y: String ? n2, z: String ? n3): Bool with (not (n2 and n3)) =
        match? (x, y, z) {
            case (a, _, _) => a == "Hello"
            case (_, b, _) => b == "World"
            case (_, _, c) => c == "!"
        }

}
