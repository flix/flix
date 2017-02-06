namespace TestConstraint {

    namespace Test01 {

        rel A(x: Unit)
        A(()).

        rel B(x: Bool)
        B(true).
        B(false).

        rel C(x: Char)
        C('a').
        C('b').
        C('c').

        rel D(x: Float32)
        D(123.456f32).
        D(987.654f32).

        rel E(x: Float64)
        E(123.456f64).
        E(987.654f64).

        rel F(x: Int8)
        F(21i8).
        F(42i8).

        rel G(x: Int16)
        G(21i16).
        G(42i16).


    }

}