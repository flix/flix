mod InlinerBenchmarkNormal {

    pub def map(f: a -> b, l: List[a]) : List[b] = {
        def mp(xs, acc) = match xs {
            case Nil     => rev(acc)
            case z :: zs => mp(zs, f(z) :: acc)
        };
        mp(l, Nil)
    }

    pub def rev(l: List[a]): List[a] = {
        def rv(xs, acc) = match xs {
            case Nil     => acc
            case z :: zs => rv(zs, z :: acc)
        };
        rv(l, Nil)
    }

    pub def range(bot: Int32, top: Int32): List[Int32] = {
        def rng(i, acc) = if (i < bot) acc else rng(i - 1, i :: acc);
        rng(top - 1, Nil)
    }

    pub def length(l: List[a]): Int32 = {
        def len(xs, acc) = match xs {
            case Nil     => acc
            case _ :: zs => len(zs, acc + 1)
        };
        len(l, 0)
    }

}

mod InlinerBenchmarkInlined {

    pub def mp(xs: List[Int32], acc: List[Int32]): List[Int32] = match xs {
        case Nil     => acc
        case z :: zs => mp(zs, z + 1 :: acc)
    }

    pub def rv(xs: List[a], acc: List[a]): List[a] = match xs {
        case Nil     => acc
        case z :: zs => rv(zs, z :: acc)
    }

    pub def ln(xs: List[a], acc: Int32): Int32 = match xs {
        case Nil     => acc
        case _ :: zs => ln(zs, acc + 1)
    }

    pub def rng(i: Int32, acc: List[Int32]): List[Int32] = if (i < 0) acc else rng(i - 1, i :: acc)

}
