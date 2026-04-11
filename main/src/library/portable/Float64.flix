pub mod Float64 {

    ///
    /// Convert `x` to an `Option[BigDecimal]`.
    ///
    /// Returns `None` if `x` is NaN or infinity.
    ///
    pub def tryToBigDecimal(x: Float64): Option[BigDecimal] =
        if (Float64.isFinite(x))
            BigDecimal.fromString(ToString.toString(x))
        else
            None

}
