namespace Test/Exp/Interpolation {

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
        "${toUpperCase(x)}${y}${z}" == "Abc"

    @test
    def interpolation13(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${x}${toUpperCase(y)}${z}" == "aBc"

    @test
    def interpolation14(): Bool =
        let x = "a";
        let y = "b";
        let z = "c";
        "${x}${y}${toUpperCase(z)}" == "abC"

    @test
    def interpolation15(): Bool =
        "${toUpperCase("w")}" == "W"

    @test
    def interpolation16(): Bool =
        "${toLowerCase("W")}" == "w"

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
        let x = Red;
        "${x}" == "r"

    @test
    def interpolation27(): Bool =
        let x = Green;
        "${x}" == "g"

    @test
    def interpolation28(): Bool =
        let x = Blue;
        "${x}" == "b"

    @test
    def interpolation29(): Bool =
        let x = Red;
        let y = Green;
        let z = Blue;
        "${x}${y}${z}" == "rgb"

    @test
    def interpolation30(): Bool =
        let x = None : Option[Int32];
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
        let x = Ok(123): Result[Int32, Int32];
        "${x}" == "Ok(123)"

    @test
    def interpolation34(): Bool =
        let x = Err(123): Result[Int32, Int32];
        "${x}" == "Err(123)"

    @test
    def interpolation35(): Bool =
        let x = Ok(Some(123)): Result[Option[Int], Int32];
        "${x}" == "Ok(Some(123))"

    @test
    def interpolation36(): Bool =
        let x = "abc";
        "\"${x}\"" == "\"abc\""

    pub enum Color {
        case Red,
        case Green,
        case Blue
    }

    pub enum Option[t] {
        case None,
        case Some(t)
    }

    pub enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

    def toLowerCase(s: String): String =
        import java.lang.String.toLowerCase();
        s.toLowerCase() as & Pure

    def toUpperCase(s: String): String =
        import java.lang.String.toUpperCase();
        s.toUpperCase() as & Pure

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
///        import java.lang.Integer:toString(Int32);
///        x.toString() as & Pure
///}
///
///instance ToString[String] {
///    def toString(x: String): String = x
///}

instance ToString[Test/Exp/Interpolation.Color] {
    pub def toString(c: Test/Exp/Interpolation.Color): String =
        use Test/Exp/Interpolation.Color.Red;
        use Test/Exp/Interpolation.Color.Green;
        use Test/Exp/Interpolation.Color.Blue;
        match c {
            case Red   => "r"
            case Green => "g"
            case Blue  => "b"
        }
}

instance ToString[Test/Exp/Interpolation.Option[a]] with ToString[a] {
    pub def toString(o: Test/Exp/Interpolation.Option[a]): String =
        use Test/Exp/Interpolation.Option.None;
        use Test/Exp/Interpolation.Option.Some;
        match o {
            case None       => "None"
            case Some(v)    => "Some(${v})"
        }
}

instance ToString[Test/Exp/Interpolation.Result[t, e]] with ToString[t], ToString[e] {
    pub def toString(r: Test/Exp/Interpolation.Result[t, e]): String =
        use Test/Exp/Interpolation.Result.Ok;
        use Test/Exp/Interpolation.Result.Err;
        match r {
            case Ok(v)  => "Ok(${v})"
            case Err(v) => "Err(${v})"
        }
}
