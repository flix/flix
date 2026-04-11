mod Api {
    eff HostEcho {
        def echo(s: String): String
    }

    @Export
    pub def add(x: Int32, y: Int32): Int32 = x + y

    @Export
    pub def fortyTwo(): Int32 = 42

    @Export
    pub def echo(s: String): String = s

    @Export
    pub def bytesLen(a: Array[Int8, Static]): Int32 = Array.length(a)

    @Export
    pub def bytesId(a: Array[Int8, Static]): Array[Int8, Static] = a

    @Export
    pub def maybeSucc(o: Option[Int32]): Option[Int32] = match o {
        case Some(n) => Some(n + 1)
        case None => None
    }

    @Export
    pub def flipPair(pair: (Int32, String)): (String, Int32) =
        let (n, s) = pair;
        (s, n)

    @Export
    pub def halfEven(x: Int32): Result[String, Int32] =
        if (Int32.remainder(x, 2) == 0) Ok(x / 2) else Err("odd")

    @Export
    pub def badge(user: {name = String, score = Int32}): {label = String, score = Int32} =
        { score = user#score + 1, label = user#name }

    @Export
    pub def prependAnswer(xs: List[Int32]): List[Int32] =
        42 :: xs

    @Export
    pub def echoInts(xs: Array[Int32, Static]): Array[Int32, Static] = xs

    @Export
    pub def echoNames(xs: Array[String, Static]): Array[String, Static] = xs

    @Export
    pub def promoteUsers(xs: List[{name = String, score = Int32}]): List[{label = String, score = Int32}] =
        List.map(u -> { label = u#name, score = u#score + 1 }, xs)

    @Export
    pub def echoUserArray(xs: Array[{name = String, score = Int32}, Static]): Array[{name = String, score = Int32}, Static] = xs

    @Export
    pub def flipPairs(xs: List[(Int32, String)]): List[(String, Int32)] =
        List.map(pair -> {
            let (n, s) = pair;
            (s, n)
        }, xs)

    @Export
    pub def echoMaybeInts(xs: Array[Option[Int32], Static]): Array[Option[Int32], Static] = xs

    @Export
    pub def suspendEcho(s: String): String \ HostEcho = HostEcho.echo(s)
}
