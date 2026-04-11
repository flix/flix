pub mod Int8 {

    ///
    /// Convert `x` to a `BigDecimal`.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigDecimal(x: Int8): BigDecimal =
        match BigDecimal.fromString(ToString.toString(x)) {
            case Some(v) => v
            case None => bug!("Int8.toBigDecimal: unexpected parse failure.")
        }

}
