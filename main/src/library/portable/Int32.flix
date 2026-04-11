pub mod Int32 {

    ///
    /// Convert `x` to a `BigDecimal`.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigDecimal(x: Int32): BigDecimal =
        match BigDecimal.fromString(ToString.toString(x)) {
            case Some(v) => v
            case None => bug!("Int32.toBigDecimal: unexpected parse failure.")
        }

}
