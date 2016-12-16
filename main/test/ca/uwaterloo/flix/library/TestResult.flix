// TODO: Liam port to this framework.

namespace Result {

    @test
    def isOk01: Bool = isOk(Ok(1))

    @test
    def isOk02: Bool = !isOk(Err(0))

    @test
    def thisFails: Bool = false

    @test
    def getWithDefault01: Bool = getWithDefault(Ok(1), 2) == 1

    @test
    def getWithDefault02: Bool = withDefault(Ok(1), Err(2ii)) == Ok(1)

    @test
    def foldRight03: Bool = !foldRight((i, b) -> i == 2 && b, Ok(1), true)

}