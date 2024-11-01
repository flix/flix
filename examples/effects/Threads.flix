///
/// From OCaml version 5.1 chapter 12
///
mod EffectExamples.Threads {
    use Status.{Complete, Suspended};

    pub eff Exchangeable {
        pub def exchange(n: Int32): Int32
    }

    pub enum Status {
        case Complete(Int32)
        case Suspended(Int32, Int32 -> Int32)
    }

    def step(f: Unit -> Int32 \ Exchangeable): Status =
        try {
            Complete(f())
        } with Exchangeable {
            def exchange(n, k) = {
                def cont(m, c): Int32 = match c(m) {
                    case Complete(v)       => v
                    case Suspended(m1, c2) => cont(m1, c2)
                };
                Suspended(n, m -> cont(m, k))
            }
        }

    def runBoth(f: Unit -> Int32 \ Exchangeable, g: Unit -> Int32 \ Exchangeable): (Int32, Int32) =
        match (step(f), step(g)) {
            case (Complete(vf), Complete(vg)) => (vf, vg)

            case (Suspended(m1, k1), Suspended(m2, k2)) =>
                runBoth(() -> k1(m2), () -> k2(m1))

            case (Complete(vf), Suspended(m2, k2)) =>
                runBoth(checked_cast(() -> vf), () -> k2(m2))

            case (Suspended(m1, k1), Complete(vg)) =>
                runBoth(() -> k1(m1), checked_cast(() -> vg))
        }
}
