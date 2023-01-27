mod Test.Dec.RestrictableTag {

/// Simple nullary cases
restrictable enum ExprNullary[_] {
    case Cst, Var, Not, And, Or, Xor
}

/// Terms of ground types
restrictable enum ExprSimple[_] {
    case Cst(Char)
    case Var(String)
    case Not((Int32, BigInt, BigDecimal))
    case And(Float64, Float32)
    case Or(Option[Bool], (Int8, Int16))
    case Xor(Array[Array[Int64, Impure], Impure], Unit)
}

/// Polymorphic term types
pub restrictable enum ExprPoly[_][t] {
    case Cst(t)
    case Var(String)
    case Not(Option[t])
    case And(t, t)
    case Or(t, t)
    case Xor(t, t)
}

/// Terms include the index argument
restrictable enum ExprRecursive[s] {
    case Cst(Bool)
    case Var(Int32)
    case Not(ExprRecursive[s])
    case And(ExprRecursive[s], ExprRecursive[s])
    case Or(ExprRecursive[s], ExprRecursive[s])
    case Xor(ExprRecursive[s], ExprRecursive[s])
}

/// Terms are polymorphic in index and type argument
pub restrictable enum ExprPolyRecursive[s][t] {
    case Cst(Bool)
    case Var(t)
    case Not(ExprPolyRecursive[s][t])
    case And(ExprPolyRecursive[s][t], ExprPolyRecursive[s][t])
    case Or(ExprPolyRecursive[s][t], ExprPolyRecursive[s][t])
    case Xor(ExprPolyRecursive[s][t], ExprPolyRecursive[s][t])
}

/// Mutually recrusive restrictable enums
restrictable enum MutualA[s][t: MutualB] {
    case ALeaf
    case ACase(MutualB[t][s])
}

restrictable enum MutualB[s][t: MutualA] {
    case BLeaf
    case BCase(MutualA[t][s])
}

}
