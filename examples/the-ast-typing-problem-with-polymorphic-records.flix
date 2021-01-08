/// A solution to the AST typing problem with extensible records.

/// We begin with a grammar for expressions.
/// The definition of Exp is polymorphic in a record type that
/// allows the AST to be decorated with additional fields.
enum Exp[r] {
    case True,
    case False,
    case Cst({value: Int | r}),
    case Add({exp1: Exp[{| r}], exp2: Exp[{| r}] | r}),
    case Ite({exp1: Exp[{| r}], exp2: Exp[{| r}], exp3: Exp[{| r}] | r})
}

/// Next, we define a grammar of types:
enum Type {
    case TBool,
    case TInt
}

/// We can now write a function that given an expression extended
/// with a tpe: Type field returns its type!
def typeOf[r](e: Exp[{tpe: Type | r}]): Type = match e {
    case True   => TBool
    case False  => TBool
    case Cst(_) => TInt
    case Add(i) => i.tpe
    case Ite(i) => i.tpe
}

/// We can write a function that takes an untyped expression
/// and returns a typed expression decorated with the type.
/// For simplicity, the actual checks have been omitted.
def typeCheck(e: Exp[{}]): Exp[{tpe: Type}] = match e {
    case True   => True
    case False  => False
    case Cst(i) => Cst({value = i.value, tpe = TInt})
    case Add(i) =>
        let e1 = typeCheck(i.exp1);
        let e2 = typeCheck(i.exp2);
            Add({exp1 = e1, exp2 = e2, tpe = TInt})
    case Ite(i) =>
      let e1 = typeCheck(i.exp1);
      let e2 = typeCheck(i.exp2);
      let e3 = typeCheck(i.exp2);
        Ite({exp1 = e1, exp2 = e2, exp3 = e3, tpe = typeOf(e2)})
}

/// We can now type check a simple expression:
def main(_: Array[String]): Int32 & Impure =
    let e = Ite({exp1 = True,
                 exp2 = Cst({value = 123}),
                 exp3 = Cst({value = 456})});
    typeOf(typeCheck(e));
    0 as & Impure // exit code

/// We can extend the function above to be one that is polymorphic
/// in whatever other fields an expression may be decorated with:
def typeCheck2[r](e: Exp[{| r}]): Exp[{tpe: Type | r}] = match e {
    case True   => True
    case False  => False
    case Cst(i) => Cst({ +tpe = TInt | { value = i.value | i}})
    case Add(i) =>
        let e1 = typeCheck2(i.exp1);
        let e2 = typeCheck2(i.exp2);
            Add({ +tpe = TInt | {exp1 = e1, exp2 = e2 | i} })
    case Ite(i) =>
      let e1 = typeCheck2(i.exp1);
      let e2 = typeCheck2(i.exp2);
      let e3 = typeCheck2(i.exp3);
        Ite({ +tpe = TInt | {exp1 = e1, exp2 = e2, exp3 = e3 | i} })
}
