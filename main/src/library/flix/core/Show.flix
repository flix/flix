// MATT copyright
namespace Show {
    class Show[a] {
        pub def show(x: a): String
    }

    instance Show[String] {
        def show(x: String): String = x
    }

    instance Show[Int8] {
        def show(x: Int8): String = Int8.toString(x)
    }

    instance Show[Int16] {
        def show(x: Int16): String = Int16.toString(x)
    }

    instance Show[Int32] {
        def show(x: Int32): String = Int32.toString(x)
    }

    instance Show[Int64] {
        def show(x: Int64): String = Int64.toString(x)
    }

    instance Show[BigInt] {
        def show(x: BigInt): String = BigInt.toString(x)
    }

    instance Show[Float32] {
        def show(x: Float32): String = Float32.toString(x)
    }

    instance Show[Float64] {
        def show(x: Float64): String = Float64.toString(x)
    }
}