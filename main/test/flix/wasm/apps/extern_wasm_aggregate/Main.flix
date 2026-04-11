mod Api {
    extern wasm(interface = "host:demo/basic@0.1.0", func = "echo-text")
    def hostEchoText(s: String): String

    extern wasm(interface = "host:demo/basic@0.1.0", func = "bytes-id")
    def hostBytesId(xs: Array[Int8, Static]): Array[Int8, Static]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "badge")
    def hostBadge(user: { name = String, score = Int32 }): { label = String, score = Int32 }

    extern wasm(interface = "host:demo/basic@0.1.0", func = "prepend")
    def hostPrepend(xs: List[Int32]): List[Int32]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "echo-ints")
    def hostEchoInts(xs: Array[Int32, Static]): Array[Int32, Static]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "echo-names")
    def hostEchoNames(xs: Array[String, Static]): Array[String, Static]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "flip")
    def hostFlip(x: (String, Int32)): (Int32, String)

    extern wasm(interface = "host:demo/basic@0.1.0", func = "flip-pairs")
    def hostFlipPairs(xs: List[(Int32, String)]): List[(String, Int32)]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "maybe-inc")
    def hostMaybeInc(x: Option[Int32]): Option[Int32]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "echo-maybe-ints")
    def hostEchoMaybeInts(xs: Array[Option[Int32], Static]): Array[Option[Int32], Static]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "half-even")
    def hostHalfEven(x: Int32): Result[String, Int32]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "promote-users")
    def hostPromoteUsers(xs: List[{ name = String, score = Int32 }]): List[{ label = String, score = Int32 }]

    extern wasm(interface = "host:demo/basic@0.1.0", func = "echo-user-array")
    def hostEchoUserArray(xs: Array[{ name = String, score = Int32 }, Static]): Array[{ name = String, score = Int32 }, Static]

    @Export
    pub def callEchoText(s: String): String = hostEchoText(s)

    @Export
    pub def callBytesId(xs: Array[Int8, Static]): Array[Int8, Static] = hostBytesId(xs)

    @Export
    pub def callBadge(user: { name = String, score = Int32 }): { label = String, score = Int32 } = hostBadge(user)

    @Export
    pub def callPrepend(xs: List[Int32]): List[Int32] = hostPrepend(xs)

    @Export
    pub def callEchoInts(xs: Array[Int32, Static]): Array[Int32, Static] = hostEchoInts(xs)

    @Export
    pub def callEchoNames(xs: Array[String, Static]): Array[String, Static] = hostEchoNames(xs)

    @Export
    pub def callFlip(x: (String, Int32)): (Int32, String) = hostFlip(x)

    @Export
    pub def callFlipPairs(xs: List[(Int32, String)]): List[(String, Int32)] = hostFlipPairs(xs)

    @Export
    pub def callMaybeInc(x: Option[Int32]): Option[Int32] = hostMaybeInc(x)

    @Export
    pub def callEchoMaybeInts(xs: Array[Option[Int32], Static]): Array[Option[Int32], Static] = hostEchoMaybeInts(xs)

    @Export
    pub def callHalfEven(x: Int32): Result[String, Int32] = hostHalfEven(x)

    @Export
    pub def callPromoteUsers(xs: List[{ name = String, score = Int32 }]): List[{ label = String, score = Int32 }] = hostPromoteUsers(xs)

    @Export
    pub def callEchoUserArray(xs: Array[{ name = String, score = Int32 }, Static]): Array[{ name = String, score = Int32 }, Static] = hostEchoUserArray(xs)
}
