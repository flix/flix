mod Test.Exp.DebugInterpolator {

    @CompileTest
    def debugInterpolator01(): String = d"Hello World"

    @CompileTest
    def debugInterpolator02(): String = d"1 + 1 is ${1 + 1}"

}
