mod Test.Exp.Interpolation {

    @test
    def interpolation01(): Bool =
        let x = "a";
        "${x}" == "a"

    @test
    def interpolation02(): Bool =
        let x = "a";
        "${x}${x}" == "aa"

    @test
    def interpolation03(): Bool =
        let x = "a";
        "${x}${x}${x}" == "aaa"

    @test
    def interpolation04(): Bool =
        let x = "a";
        "${x}x${x}y${x}" == "axaya"

    @test
    def interpolation05(): Bool =
        let x = "a";
        let y = "b";
        "${x}${y}" == "ab"

    @test
    def interpolation06(): Bool =
        let x = "a";
        let y = "b";
        "${x}${y}${x}" == "aba"

    @test
    def interpolation07(): Bool =
        let x = "a";
        let y = "b";
        "${x}${x}${y}${y}" == "aabb"

    @test
    def interpolation08(): Bool =
        let x = "a";
        let y = "b";
        "${x}${y}${x}${y}" == "abab"

    @test
    def interpolation09(): Bool =
        let x = "a";
        let y = "b";
        "${x}u${y}v${x}w${y}" == "aubvawb"

    @test
    def interpolation10(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${x}${y}${z}" == "abc"

    @test
    def interpolation11(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${z}${y}${x}" == "cba"

    @test
    def interpolation12(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${String.toUpperCase(x)}${y}${z}" == "Abc"

    @test
    def interpolation13(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${x}${String.toUpperCase(y)}${z}" == "aBc"

    @test
    def interpolation14(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${x}${y}${String.toUpperCase(z)}" == "abC"

    @test
    def interpolation15(): Bool =
        "${String.toUpperCase("w")}" == "W"

    @test
    def interpolation16(): Bool =
        "${String.toLowerCase("W")}" == "w"

    @test
    def interpolation17(): Bool =
        "$" == "$"

    @test
    def interpolation18(): Bool =
        "{}" == "{}"

    @test
    def interpolation19(): Bool =
        let x = "42";
        "USD $${x}" == "USD $42"

    @test
    def interpolation20(): Bool =
        let x = "a";
        "$${x}$" == "$a$"

    @test
    def interpolation21(): Bool =
        let x = true;
        "${x}" == "true"

    @test
    def interpolation22(): Bool =
        let x = false;
        "${x}" == "false"

    @test
    def interpolation23(): Bool =
        let x = 1;
        "${x}" == "1"

    @test
    def interpolation24(): Bool =
        let x = 123;
        let y = 456;
        "${x}${y}" == "123456"

    @test
    def interpolation25(): Bool =
        let x = 123;
        let y = 456;
        "${y - x}" == "333"

    @test
    def interpolation26(): Bool =
        let x = Color.Red;
        "${x}" == "r"

    @test
    def interpolation27(): Bool =
        let x = Color.Green;
        "${x}" == "g"

    @test
    def interpolation28(): Bool =
        let x = Color.Blue;
        "${x}" == "b"

    @test
    def interpolation29(): Bool =
        let x = Color.Red;
        let y = Color.Green;
        let z = Color.Blue;
        "${x}${y}${z}" == "rgb"

    @test
    def interpolation30(): Bool =
        let x = (None : Option[Int32]);
        "${x}" == "None"

    @test
    def interpolation31(): Bool =
        let x = Some(123);
        "${x}" == "Some(123)"

    @test
    def interpolation32(): Bool =
        let x = Some(Some(123));
        "${x}" == "Some(Some(123))"

    @test
    def interpolation33(): Bool =
        let x = (Ok(123): Result[Int32, Int32]);
        "${x}" == "Ok(123)"

    @test
    def interpolation34(): Bool =
        let x = (Err(123): Result[Int32, Int32]);
        "${x}" == "Err(123)"

    @test
    def interpolation35(): Bool =
        let x = (Ok(Some(123)): Result[Int32, Option[Int32]]);
        "${x}" == "Ok(Some(123))"

    @test
    def interpolation36(): Bool =
        let x = "abc";
        "\"${x}\"" == "\"abc\""

    @test
    def interpolationDebug01(): Bool =
        "%{123}" == "123"

    @test
    def interpolationDebug02(): Bool =
        let x = "123";
        "%{x}" == "\"123\""

    @test
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

///pub trait ToString[a] {
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
