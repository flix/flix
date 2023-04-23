/// A solution to the AST typing problem with extensible records.

/// We begin with a grammar for expressions.
/// The definition of Exp is polymorphic in a record type that
/// allows the AST to be decorated with additional fields.
enum Expr[r: RecordRow] {
    case True,
    case False,
    case Cst({value = Int32 | r}),
    case Add({exp1 = Expr[r], exp2 = Expr[r] | r}),
    case Ite({exp1 = Expr[r], exp2 = Expr[r], exp3 = Expr[r] | r})
}

/// Next, we define a grammar of types:
enum Type {
    case TBool,
    case TInt
}

/// We can now write a function that given an expression extended
/// with a tpe: Type field returns its type!
def typeOf(e: Expr[(tpe = Type | r)]): Type = match e {
    case Expr.True   => Type.TBool
    case Expr.False  => Type.TBool
    case Expr.Cst(_) => Type.TInt
    case Expr.Add(i) => i.tpe
    case Expr.Ite(i) => i.tpe
}

/// We can write a function that takes an untyped expression
/// and returns a typed expression decorated with the type.
/// For simplicity, the actual checks have been omitted.
def typeCheck(e: Expr[()]): Expr[(tpe = Type)] = match e {
    case Expr.True   => Expr.True
    case Expr.False  => Expr.False
    case Expr.Cst(i) => Expr.Cst({value = i.value, tpe = Type.TInt})
    case Expr.Add(i) =>
        let e1 = typeCheck(i.exp1);
        let e2 = typeCheck(i.exp2);
            Expr.Add({exp1 = e1, exp2 = e2, tpe = Type.TInt})
    case Expr.Ite(i) =>
      let e1 = typeCheck(i.exp1);
      let e2 = typeCheck(i.exp2);
      let e3 = typeCheck(i.exp2);
        Expr.Ite({exp1 = e1, exp2 = e2, exp3 = e3, tpe = typeOf(e2)})
}

/// We can now type check a simple expression:
def run(): Type =
    let e = Expr.Ite({exp1 = Expr.True,
                 exp2 = Expr.Cst({value = 123}),
                 exp3 = Expr.Cst({value = 456})});
    typeOf(typeCheck(e))

/// We can extend the function above to be one that is polymorphic
/// in whatever other fields an expression may be decorated with:
def typeCheck2(e: Expr[r]): Expr[(tpe = Type | r)] = match e {
    case Expr.True   => Expr.True
    case Expr.False  => Expr.False
    case Expr.Cst(i) => Expr.Cst({ +tpe = Type.TInt | { value = i.value | i}})
    case Expr.Add(i) =>
        let e1 = typeCheck2(i.exp1);
        let e2 = typeCheck2(i.exp2);
            Expr.Add({ +tpe = Type.TInt | {exp1 = e1, exp2 = e2 | i} })
    case Expr.Ite(i) =>
      let e1 = typeCheck2(i.exp1);
      let e2 = typeCheck2(i.exp2);
      let e3 = typeCheck2(i.exp3);
        Expr.Ite({ +tpe = Type.TInt | {exp1 = e1, exp2 = e2, exp3 = e3 | i} })
}
