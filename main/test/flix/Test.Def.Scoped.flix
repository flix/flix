namespace Test/Def/Scoped {

    // FUNCTIONALITY TESTS
    pub def binaryLosesScopedness(scoped x: Int): Int = x + 1

    pub def ignoredParamScopednessIrrelevant(scoped _x: Int): Int = 3

    pub def sortList(list: List[a]): List[a] & Impure = {
      let scoped array = toArray(list);
      toList(array)
    }

    pub def reset(scoped r: Ref[a]): Unit & Impure = {
      r := default
    }

    pub def scopedLambdaParam(): Int -> Int = scoped x -> x + 1

    pub def nestedLambdas(): Int -> Int -> Int = scoped x -> scoped y -> (x: Int) + (y: Int)

    pub def scopedLambda(): Int =
        let scoped f = x -> x + 1;
        let scoped g = y -> f(y);
        g(4)

    pub def letInLambda(): Int -> Int =
        x -> {
            let scoped y = 5;
            x + y
        }

    // HELPERS
    def toArray(list: List[a]): Array[a] & Impure = ???
    def toList(array: Array[a]): List[a] & Impure = ???
    pub opaque type List[a] = a // fake type

    @test
    def empty(): Bool = true
}