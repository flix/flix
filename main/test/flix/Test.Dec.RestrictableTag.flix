pub restrictable enum ExprPoly[_][t] {
    case Cst(t)
    case Var(String)
    case Not(Option[t])
    case And(t, t)
    case Or(t, t)
    case Xor(t, t)
}

restrictable enum ExprRecursive[s] {
    case Cst(Bool)
    case Var(Int32)
    case Not(ExprRecursive[s])
    case And(ExprRecursive[s], ExprRecursive[s])
    case Or(ExprRecursive[s], ExprRecursive[s])
    case Xor(ExprRecursive[s], ExprRecursive[s])
}

pub restrictable enum ExprPolyRecursive[s][t] {
    case Cst(Bool)
    case Var(t)
    case Not(ExprPolyRecursive[s][t])
    case And(ExprPolyRecursive[s][t], ExprPolyRecursive[s][t])
    case Or(ExprPolyRecursive[s][t], ExprPolyRecursive[s][t])
    case Xor(ExprPolyRecursive[s][t], ExprPolyRecursive[s][t])
}

restrictable enum ExprNullary[s] {
    case Cst, Var, Not, And, Or, Xor
}

restrictable enum ExprSimple[s] {
    case Cst(Char)
    case Var(String)
    case Not((Int32, BigInt, BigDecimal))
    case And(Float64, Float32)
    case Or(Option[Bool], (Int8, Int16))
    case Xor(Array[Array[Int64, Impure], Impure], Unit)
}

restrictable enum MutualA[s][t: MutualB] {
    case ALeaf
    case ACase(MutualB[t][s])
}

restrictable enum MutualB[s][t: MutualA] {
    case BLeaf
    case BCase(MutualA[t][s])
}
