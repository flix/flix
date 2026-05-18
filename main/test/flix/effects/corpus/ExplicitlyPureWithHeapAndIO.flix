def explicitlyPureWithHeapAndIO(s: MutSet[Int32, r]): Unit \ { } =
    let n = MutSet.size(s);
    println(n)
