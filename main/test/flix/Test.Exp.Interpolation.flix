mod Test.Exp.Interpolation {

    use Assert.assertEq

    @Test
    def interpolation01(): Unit \ Assert =
        let x = "a";
        assertEq(expected = "a", "${x}")

    @Test
    def interpolation02(): Unit \ Assert =
        let x = "a";
        assertEq(expected = "aa", "${x}${x}")

    @Test
    def interpolation03(): Unit \ Assert =
        let x = "a";
        assertEq(expected = "aaa", "${x}${x}${x}")

    @Test
    def interpolation04(): Unit \ Assert =
        let x = "a";
        assertEq(expected = "axaya", "${x}x${x}y${x}")

    @Test
    def interpolation05(): Unit \ Assert =
        let x = "a";
        let y = "b";
        assertEq(expected = "ab", "${x}${y}")

    @Test
    def interpolation06(): Unit \ Assert =
        let x = "a";
        let y = "b";
        assertEq(expected = "aba", "${x}${y}${x}")

    @Test
    def interpolation07(): Unit \ Assert =
        let x = "a";
        let y = "b";
        assertEq(expected = "aabb", "${x}${x}${y}${y}")

    @Test
    def interpolation08(): Unit \ Assert =
        let x = "a";
        let y = "b";
        assertEq(expected = "abab", "${x}${y}${x}${y}")

    @Test
    def interpolation09(): Unit \ Assert =
        let x = "a";
        let y = "b";
        assertEq(expected = "aubvawb", "${x}u${y}v${x}w${y}")

    @Test
    def interpolation10(): Unit \ Assert =
        let x = "a";
        let y = "b";
        let z = "c";
        assertEq(expected = "abc", "${x}${y}${z}")

    @Test
    def interpolation11(): Unit \ Assert =
        let x = "a";
        let y = "b";
        let z = "c";
        assertEq(expected = "cba", "${z}${y}${x}")

    @Test
    def interpolation12(): Unit \ Assert =
        let x = "a";
        let y = "b";
        let z = "c";
        assertEq(expected = "Abc", "${String.toUpperCase(x)}${y}${z}")

    @Test
    def interpolation13(): Unit \ Assert =
        let x = "a";
        let y = "b";
        let z = "c";
        assertEq(expected = "aBc", "${x}${String.toUpperCase(y)}${z}")

    @Test
    def interpolation14(): Unit \ Assert =
        let x = "a";
        let y = "b";
        let z = "c";
        assertEq(expected = "abC", "${x}${y}${String.toUpperCase(z)}")

    @Test
    def interpolation15(): Unit \ Assert =
        assertEq(expected = "W", "${String.toUpperCase("w")}")

    @Test
    def interpolation16(): Unit \ Assert =
        assertEq(expected = "w", "${String.toLowerCase("W")}")

    @Test
    def interpolation17(): Unit \ Assert =
        assertEq(expected = "$", "$")

    @Test
    def interpolation18(): Unit \ Assert =
        assertEq(expected = "{}", "{}")

    @Test
    def interpolation19(): Unit \ Assert =
        let x = "42";
        assertEq(expected = "USD $42", "USD $${x}")

    @Test
    def interpolation20(): Unit \ Assert =
        let x = "a";
        assertEq(expected = "$a$", "$${x}$")

    @Test
    def interpolation21(): Unit \ Assert =
        let x = true;
        assertEq(expected = "true", "${x}")

    @Test
    def interpolation22(): Unit \ Assert =
        let x = false;
        assertEq(expected = "false", "${x}")

    @Test
    def interpolation23(): Unit \ Assert =
        let x = 1;
        assertEq(expected = "1", "${x}")

    @Test
    def interpolation24(): Unit \ Assert =
        let x = 123;
        let y = 456;
        assertEq(expected = "123456", "${x}${y}")

    @Test
    def interpolation25(): Unit \ Assert =
        let x = 123;
        let y = 456;
        assertEq(expected = "333", "${y - x}")

    @Test
    def interpolation26(): Unit \ Assert =
        let x = Color.Red;
        assertEq(expected = "r", "${x}")

    @Test
    def interpolation27(): Unit \ Assert =
        let x = Color.Green;
        assertEq(expected = "g", "${x}")

    @Test
    def interpolation28(): Unit \ Assert =
        let x = Color.Blue;
        assertEq(expected = "b", "${x}")

    @Test
    def interpolation29(): Unit \ Assert =
        let x = Color.Red;
        let y = Color.Green;
        let z = Color.Blue;
        assertEq(expected = "rgb", "${x}${y}${z}")

    @Test
    def interpolation30(): Unit \ Assert =
        let x = (None : Option[Int32]);
        assertEq(expected = "None", "${x}")

    @Test
    def interpolation31(): Unit \ Assert =
        let x = Some(123);
        assertEq(expected = "Some(123)", "${x}")

    @Test
    def interpolation32(): Unit \ Assert =
        let x = Some(Some(123));
        assertEq(expected = "Some(Some(123))", "${x}")

    @Test
    def interpolation33(): Unit \ Assert =
        let x = (Ok(123): Result[Int32, Int32]);
        assertEq(expected = "Ok(123)", "${x}")

    @Test
    def interpolation34(): Unit \ Assert =
        let x = (Err(123): Result[Int32, Int32]);
        assertEq(expected = "Err(123)", "${x}")

    @Test
    def interpolation35(): Unit \ Assert =
        let x = (Ok(Some(123)): Result[Int32, Option[Int32]]);
        assertEq(expected = "Ok(Some(123))", "${x}")

    @Test
    def interpolation36(): Unit \ Assert =
        let x = "abc";
        assertEq(expected = "\"abc\"", "\"${x}\"")

    @Test
    def interpolation37(): Unit \ Assert =
        assertEq(expected = String.concat("$", "{"), "\${")

    @Test
    def interpolation38(): Unit \ Assert =
        assertEq(expected = String.concat("\\$", "{"), "\\\${")

    pub enum Color {
        case Red,
        case Green,
        case Blue
    }

    instance ToString[Color] {
        pub def toString(c: Color): String =
            match c {
                case Color.Red   => "r"
                case Color.Green => "g"
                case Color.Blue  => "b"
            }
    }

    pub enum EnumSansToString {
        case CaseSansToString(Int32)
    }

}
