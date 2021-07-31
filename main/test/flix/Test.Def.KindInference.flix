namespace Test/Def/KindInference {

    pub enum Func[ef : Bool] {
        case Function(Int -> Int & ef)
    }

    pub def func[ef: Bool](f: Int -> Int & ef, g: Func[ef]): Int = ???

    pub def project3(p: PredSym, ts: f[(t1, t2, t3)]): Datalog[v] with Boxable[t1], Boxable[t2], Boxable[t3], Foldable[f] = ???

    pub enum PredSym
    pub class Boxable[a]
    pub enum Datalog[_a]
    pub class Foldable[a: Type -> Type]

    @test
    def empty(): Bool = true
}
