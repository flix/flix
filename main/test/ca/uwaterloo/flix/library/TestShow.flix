// MATT copyright
namespace TestShow {
    use Show.show; // MATT seems like UseDef correctly imports sig from class but not from instances

    @test
    def testShowString(): Bool = show("abcd") == "abcd"

    @test
    def testShowInt8(): Bool = show(123i8) == "123"

    @test
    def testShowInt16(): Bool = show(123i16) == "123"

    @test
    def testShowInt32(): Bool = show(123i32) == "123"

    @test
    def testShowInt64(): Bool = show(123i64) == "123"

    @test
    def testShowFloat32(): Bool = show(123.0f32) == "123.0"

    @test
    def testShowFloat64(): Bool = show(123.0f64) == "123.0"
}
