namespace Random {

    pub enum Random {
        case Random(##java.util.Random)
    }

    pub def new(): Random = Random(native new java.util.Random())

    pub def nextBool(r: Random): Bool =
        let Random(o) = r;
        native method java.util.Random.nextBoolean(o : ##java.util.Random) as Bool

}
