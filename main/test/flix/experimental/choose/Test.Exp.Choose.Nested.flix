restrictable enum Option[_][t] {
    case Some(t)
    case None
}

restrictable enum Expr[s][t] {
    case Cst(Option[t, Char])
    case Var(String)
    case Not((Int32, BigInt, BigDecimal))
    case And(Option[t, Option[t, Float64]], Float32)
    case Or(Option[t, Bool], (Int8, Int16))
    case Xor(Array[Array[Option[t, Int64], Impure], Impure], Unit)
}
