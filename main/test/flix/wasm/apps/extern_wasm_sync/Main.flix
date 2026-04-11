mod Api {
    extern wasm(interface = "host:math/basic@0.1.0", func = "cos")
    def hostCos(x: Float64): Float64

    extern wasm(interface = "host:math/basic@0.1.0", func = "neg")
    def hostNeg(x: Int32): Int32

    extern wasm(interface = "host:math/basic@0.1.0", func = "iseven")
    def hostIsEven(x: Int32): Bool

    @Export
    pub def callCos(x: Float64): Float64 = hostCos(x)

    @Export
    pub def callNeg(x: Int32): Int32 = hostNeg(x)

    @Export
    pub def callIsEven(x: Int32): Bool = hostIsEven(x)
}
