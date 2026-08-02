mod Test.Exp.Jvm.NewObject.Overload {
    import java.lang.Appendable
    import java.lang.CharSequence
    
    @CompileTest
    def mk(): Appendable \ IO = {
        new Appendable {
            def append(_this: Appendable, seq: CharSequence): Appendable = ???
            def append(_this: Appendable, char: Char): Appendable = ???
            def append(_this: Appendable, seq: CharSequence, start: Int32, end: Int32): Appendable = ???
        }
    }
}
