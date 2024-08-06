mod Test.Import {

    import java.lang.{StringBuffer, CharSequence => ASeqOfChars}
    import java.lang.{StringBuffer => RenamedBuffer1}
    import java.sql.Time

    // Doesn't cause an error because different names from above
    import java.lang.{StringBuffer => RenamedBuffer2, CharSequence}

    @test
    def testImport01(): Bool \ IO =
        let sb = new StringBuffer("Hello world");
        let subStr = unchecked_cast(sb.subSequence(6, 11) as String);
        Assert.eq(subStr, "world")

    @test
    def testImport02(): Bool \ IO =
        let sb = new StringBuffer("Hello world");
        let subStr = unchecked_cast(sb.subSequence(6, 11) as String);
        Assert.eq(subStr, "world")
}

mod Test.Import {
    // Doesn't cause an error because different scope from above
    import java.lang.{StringBuffer => RenamedBuffer1}
    import java.lang.CharSequence
    import java.sql.Time

    @test
    def testImport03(): Bool \ IO =
        let sb = new RenamedBuffer1("Hello world");
        let subStr = unchecked_cast(sb.subSequence(6, 11) as String);
        Assert.eq(subStr, "world")

    @test
    def testImport04(): Time \ IO =
        new Time(1000i64)
    }

mod Test.Import {
    // Doesn't cause an error because different scope from above
    import java.lang.CharSequence
}
