namespace Test {
    def ppBool(b: Bool): String = match b {
        case true => "true"
        case false => "false"
    }

    pub def test(name: String, f: () -> Bool): Bool & Impure = {
        Console.print("Testing ::: ");
        Console.print(name);
        Console.print(": ");
        let res = f();
        Console.printLine(ppBool(res));
        res
    }

    pub def runAllTests(): Bool & Impure = {
        Manifest/Test.allTests()
    }
}