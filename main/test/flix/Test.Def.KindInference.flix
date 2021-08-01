namespace Test/Def/KindInference {

    namespace Test01 {
        pub enum Func[ef : Bool] {
            case Function(Int -> Int & ef)
        }
        pub def func(f: Int -> Int & ef, g: Func[ef]): Int = ???
    }

    namespace Test02 {
        pub def project3(p: PredSym, ts: f[(t1, t2, t3)]): Datalog[v] with Boxable[t1], Boxable[t2], Boxable[t3], Foldable[f] = ???

        pub enum PredSym
        pub class Boxable[a]
        pub enum Datalog[_a]
        pub class Foldable[a: Type -> Type]
    }

    namespace Test03 {
        pub enum E[a: Record] {
            case C(a)
        }
    }

    @test
    def empty(): Bool = true
}
