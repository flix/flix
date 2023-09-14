mod Test.Exp.Interpolation {

    @Test
    def interpolation01(): Bool =
        let x = "a";
        "${x}" == "a"

    @Test
    def interpolation02(): Bool =
        let x = "a";
        "${x}${x}" == "aa"

    @Test
    def interpolation03(): Bool =
        let x = "a";
        "${x}${x}${x}" == "aaa"

    @Test
    def interpolation04(): Bool =
        let x = "a";
        "${x}x${x}y${x}" == "axaya"

    @Test
    def interpolation05(): Bool =
        let x = "a";
        let y = "b";
        "${x}${y}" == "ab"

    @Test
    def interpolation06(): Bool =
        let x = "a";
        let y = "b";
        "${x}${y}${x}" == "aba"

    @Test
    def interpolation07(): Bool =
        let x = "a";
        let y = "b";
        "${x}${x}${y}${y}" == "aabb"

    @Test
    def interpolation08(): Bool =
        let x = "a";
        let y = "b";
        "${x}${y}${x}${y}" == "abab"

    @Test
    def interpolation09(): Bool =
        let x = "a";
        let y = "b";
        "${x}u${y}v${x}w${y}" == "aubvawb"

    @Test
    def interpolation10(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${x}${y}${z}" == "abc"

    @Test
    def interpolation11(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${z}${y}${x}" == "cba"

    @Test
    def interpolation12(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${String.toUpperCase(x)}${y}${z}" == "Abc"

    @Test
    def interpolation13(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${x}${String.toUpperCase(y)}${z}" == "aBc"

    @Test
    def interpolation14(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${x}${y}${String.toUpperCase(z)}" == "abC"

    @Test
    def interpolation15(): Bool =
        "${String.toUpperCase("w")}" == "W"

    @Test
    def interpolation16(): Bool =
        "${String.toLowerCase("W")}" == "w"

    @Test
    def interpolation17(): Bool =
        "$" == "$"

    @Test
    def interpolation18(): Bool =
        "{}" == "{}"

    @Test
    def interpolation19(): Bool =
        let x = "42";
        "USD $${x}" == "USD $42"

    @Test
    def interpolation20(): Bool =
        let x = "a";
        "$${x}$" == "$a$"

    @Test
    def interpolation21(): Bool =
        let x = true;
        "${x}" == "true"

    @Test
    def interpolation22(): Bool =
        let x = false;
        "${x}" == "false"

    @Test
    def interpolation23(): Bool =
        let x = 1;
        "${x}" == "1"

    @Test
    def interpolation24(): Bool =
        let x = 123;
        let y = 456;
        "${x}${y}" == "123456"

    @Test
    def interpolation25(): Bool =
        let x = 123;
        let y = 456;
        "${y - x}" == "333"

    @Test
    def interpolation26(): Bool =
        let x = Color.Red;
        "${x}" == "r"

    @Test
    def interpolation27(): Bool =
        let x = Color.Green;
        "${x}" == "g"

    @Test
    def interpolation28(): Bool =
        let x = Color.Blue;
        "${x}" == "b"

    @Test
    def interpolation29(): Bool =
        let x = Color.Red;
        let y = Color.Green;
        let z = Color.Blue;
        "${x}${y}${z}" == "rgb"

    @Test
    def interpolation30(): Bool =
        let x = None : Option[Int32];
        "${x}" == "None"

    @Test
    def interpolation31(): Bool =
        let x = Some(123);
        "${x}" == "Some(123)"

    @Test
    def interpolation32(): Bool =
        let x = Some(Some(123));
        "${x}" == "Some(Some(123))"

    @Test
    def interpolation33(): Bool =
        let x = Ok(123): Result[Int32, Int32];
        "${x}" == "Ok(123)"

    @Test
    def interpolation34(): Bool =
        let x = Err(123): Result[Int32, Int32];
        "${x}" == "Err(123)"

    @Test
    def interpolation35(): Bool =
        let x = Ok(Some(123)): Result[Int32, Option[Int32]];
        "${x}" == "Ok(Some(123))"

    @Test
    def interpolation36(): Bool =
        let x = "abc";
        "\"${x}\"" == "\"abc\""

    @Test
    def interpolationDebug01(): Bool =
        "%{123}" == "123"

    @Test
    def interpolationDebug02(): Bool =
        let x = "123";
        "%{x}" == "\"123\""

    @Test
    def interpolationDebug03(): Bool =
        let x = EnumSansToString.CaseSansToString(123);
        "my object is %{x}" == "my object is CaseSansToString(123)"

    pub enum Color {
        case Red,
        case Green,
        case Blue
    }

    pub enum EnumSansToString {
        case CaseSansToString(Int32)
    }

}

///pub class ToString[a] {
///    pub def toString(x: a): String
///}
///
///instance ToString[Bool] {
///    def toString(x: Bool): String = match x {
///        case true => "true"
///        case false => "false"
///    }
///}
///
///instance ToString[Int32] {
///    def toString(x: Int32): String =
///        import static java.lang.Integer.toString(Int32): String \ {};
///        x.toString()
///}
///
///instance ToString[String] {
///    def toString(x: String): String = x
///}

instance ToString[Test.Exp.Interpolation.Color] {
    pub def toString(c: Test.Exp.Interpolation.Color): String =
        use Test.Exp.Interpolation.Color.Red;
        use Test.Exp.Interpolation.Color.Green;
        use Test.Exp.Interpolation.Color.Blue;
        match c {
            case Red   => "r"
            case Green => "g"
            case Blue  => "b"
        }
}
