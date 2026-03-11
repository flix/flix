def parMap(f: a -> b, l: List[a]): List[b] = match l {
    case Nil     => Nil
    case x :: xs =>
        par (r <- f(x); rs <- parMap(f, xs))
            yield r :: rs
}

def main(): Unit \ IO =
    let l = List.range(1, 100);
    println(parMap(x -> x + 1, l))
