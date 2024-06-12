///
/// From OCaml version 5.1 chapter 12
///
mod EffectExamples.Threads {
    use Status.{Complete, Suspended};

    pub eff Exchangable {
        pub def exchange(n: Int32): Int32
    }

    pub enum Status[a] {
        case Complete(a)
        case Suspended(a, a -> a)
    }

    def step(f: Unit -> a \ Exchangable): Status[a] =
        try {
            Complete(f())
        } with Exchangable {
            def exchange(n, k) = {
                def cont(m, c) = match c(m) {
                    case Complete(v)       => v
                    case Suspended(m1, c2) => cont(m1, c2)
                };
                Suspended(n, m -> cont(m, k))
            }
        }

    def runBoth(f: Unit -> a \ Exchangable, g: Unit -> a \ Exchangable): (a, a) =
        match (step(f), step(g)) {
            case (Complete(vf), Complete(vg)) => (vf, vg)
            case (Suspended(m1, k1), Suspended(m2, k2)) =>
                runBoth(() -> k1(m2), () -> k2(m1))
        }
}
