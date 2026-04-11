pub mod Float32 {

    ///
    /// Convert `x` to an `Option[BigDecimal]`.
    ///
    /// Returns `None` if `x` is NaN or infinity.
    ///
    pub def tryToBigDecimal(x: Float32): Option[BigDecimal] =
        if (Float32.isFinite(x))
            BigDecimal.fromString(ToString.toString(x))
        else
            None

}
