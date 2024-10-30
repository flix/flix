mod Test.Dec.RestrictableTag {

/// Simple nullary cases
pub restrictable enum ExprNullary[_] {
    case Cst, Var, Not, And, Or, Xor
}

/// Normal enum containing index variable
pub enum ExprWrapper[s: ExprNullary] {
    case Wrap(ExprNullary[s])
}

/// Terms of ground types
pub restrictable enum ExprSimple[_] {
    case Cst(Char)
    case Var(String)
    case Not((Int32, BigInt, BigDecimal))
    case And(Float64, Float32)
    case Or(Option[Bool], (Int8, Int16))
    case Xor(Array[Array[Int64, Univ], Univ], Unit)
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
pub restrictable enum ExprRecursive[s] {
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
pub restrictable enum MutualA[s][t: MutualB] {
    case ALeaf
    case ACase(MutualB[t][s])
}

pub restrictable enum MutualB[s][t: MutualA] {
    case BLeaf
    case BCase(MutualA[t][s])
}

/// Non-direct use of index
pub restrictable enum ExprFancy[s][t: ExprFancy] {
    case A(ExprFancy[t][s])
    case B(ExprFancy[t rvsub <ExprFancy.A>][t])
    case C(ExprFancy[s rvadd t][<ExprFancy.A>])
}

}
