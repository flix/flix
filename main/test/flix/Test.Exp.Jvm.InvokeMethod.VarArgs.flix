mod Test.Exp.Jvm.InvokeMethod.VarArgs {

    import java.nio.file.Path

    def main(): Unit \ IO =
        let foo = Path.of("hello", ...{"abc"});
        println(foo.toString())

}
