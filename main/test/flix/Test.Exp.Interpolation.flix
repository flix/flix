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

    def toLowerCase(s: String): String =
        import java.lang.String.toLowerCase();
        s.toLowerCase() as & Pure

    def toUpperCase(s: String): String =
        import java.lang.String.toUpperCase();
        s.toUpperCase() as & Pure

}

namespace Show {

    class Show[a] {
        pub def show(x: a): String
    }

    instance Show[Bool] {
        def show(x: Bool): String = match x {
            case true => "true"
            case false => "false"
        }
    }

    instance Show[String] {
        def show(x: String): String = x
    }

}
