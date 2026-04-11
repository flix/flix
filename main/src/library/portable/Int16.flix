pub mod Int16 {

    ///
    /// Convert `x` to a `BigDecimal`.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigDecimal(x: Int16): BigDecimal =
        match BigDecimal.fromString(ToString.toString(x)) {
            case Some(v) => v
            case None => bug!("Int16.toBigDecimal: unexpected parse failure.")
        }

}
