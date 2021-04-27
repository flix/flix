namespace Test/Def/KindInference {

    pub enum Func[ef : Bool] {
        case Function(Int -> Int & ef)
    }

    pub def func[ef: Bool](f: Int -> Int & ef, g: Func[ef]): Int = ???

    @test
    def empty(): Bool = true
}
