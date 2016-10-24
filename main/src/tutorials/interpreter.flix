///
/// In this tutorial we demonstrate the functional language in Flix.
///
/// We will write some interpreters and compilers for a small toy programming language.
///
/// We begin by defining the data types we will be working with.
///

///
/// We define an enum to capture the syntax of arithmetic expressions.
///
enum AExp {
    // a literal integer constant.
    case Cst(Int),

    // a binary addition expression: e1 + e2.
    case Plus(AExp, AExp),

    // a binary substraction expression: e1 - e2.
    case Minus(AExp, AExp),

    // a binary multiplication expression: e1 * e2.
    case Times(AExp, AExp),

    // a binary division expression: e1 / e2.
    case Divide(AExp, AExp),

    // a if-then-else expression: if (e1) e2 else e3.
    case IfThenElse(BExp, AExp, AExp)
}

///
/// We then define an enum to capture the syntax of boolean expressions.
///
enum BExp {
    // the true boolean literal.
    case True,

    // the false boolean literal.
    case False,

    // a logical negation expression: !e.
    case Not(BExp),

    // a logical conjunction expression: e1 && e2.
    case Conj(BExp, BExp),

    // a logical disjunction expression: e1 || e2.
    case Disj(BExp, BExp),

    // an equality of expression: e1 == e2.
    case Eq(AExp, AExp),

    // an inequality of expression: e1 != e2.
    case Neq(AExp, AExp)
}

///
/// We can now define a small interpreter for arithmetic expressions.
///
def evalAExp(e: AExp): Int = match e with {
    case Cst(i)                 => i
    case Plus(e1, e2)           => evalAExp(e1) + evalAExp(e2)
    case Minus(e1, e2)          => evalAExp(e1) - evalAExp(e2)
    case Times(e1, e2)          => evalAExp(e1) * evalAExp(e2)
    case Divide(e1, e2)         => evalAExp(e1) / evalAExp(e2)
    case IfThenElse(e1, e2, e3) =>
        let cond = evalBExp(e1);
            if (cond) evalAExp(e2) else evalAExp(e3)
}

///
/// And here is the small interpreter for boolean expressions.
///
def evalBExp(e: BExp): Bool = match e with {
    case True           => true
    case False          => false
    case Not(e)         => !evalBExp(e)
    case Conj(e1, e2)   => evalBExp(e1) && evalBExp(e2)
    case Disj(e1, e2)   => evalBExp(e1) || evalBExp(e2)
    case Eq(e1, e2)     => evalAExp(e1) == evalAExp(e2)
    case Neq(e1,e2)     => evalAExp(e1) != evalAExp(e2)
}

///
/// We test each interpreter by writing some small test.
///
/// You can run these functions by passing the `--main` argument to Flix, e.g.
///   $ flix --main testEvalAExp1
///

def testEvalAExp1: Int = evalAExp(Cst(42))
def testEvalAExp2: Int = evalAExp(Plus(Cst(42), Cst(21)))
def testEvalAExp3: Int = evalAExp(Minus(Cst(42), Cst(21)))
def testEvalAExp4: Int = evalAExp(IfThenElse(True, Cst(1), Cst(2)))
def testEvalAExp5: Int = evalAExp(IfThenElse(Neq(Cst(1), Cst(2)), Cst(42), Cst(21)))

def testEvalBExp1: Bool = evalBExp(True)
def testEvalBExp2: Bool = evalBExp(Not(True))
def testEvalBExp3: Bool = evalBExp(Conj(True, False))
def testEvalBExp4: Bool = evalBExp(Disj(True, False))
def testEvalBExp5: Bool = evalBExp(Eq(Cst(1), Cst(2)))

///
/// We now write two small compilers that translate arithmetic and boolean
/// expression into a sequence of instructions.
///

///
/// We define an enum to capture the syntax of instructions.
///
enum Inst {

    // an instruction which pushes the constant integer on the stack.
    case Push(Int),

    // an instruction which adds the two top operands on the stack.
    case Add,

    // an instruction which substracts the two top operands on the stack.
    case Sub,

    // an instruction which multiplies the two top operands on the stack.
    case Mul,

    // an instruction which divides the two top operands on the stack.
    case Div,

    // an instruction which negates the top operand on the stack.
    case Neg,

    // an instruction which computes the logical-and of the two top operands on the stack.
    case And,

    // an instruction which computes the logical-or of the two top operands on the stack.
    case Or,

    // an instruction which computes equality of the two top operands on the stack.
    case Cmp,

    // an instruction which branches based on the top operand on the stack.
    case Branch(List[Inst], List[Inst])
}

///
/// Compiles the given arithmetic expression to a sequence of instructions.
///
def compileAExp(e: AExp): List[Inst] = match e with {
    case Cst(i)         => Push(i) :: Nil
    case Plus(e1, e2)   =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            append(append(is1, is2), (Add :: Nil))
    case Minus(e1, e2)  =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            append(append(is1, is2), (Sub :: Nil))
    case Times(e1, e2)  =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            append(append(is1, is2), (Mul :: Nil))
    case Divide(e1, e2)  =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            append(append(is1, is2), (Div :: Nil))
    case IfThenElse(e1, e2, e3)  =>
        let is1 = compileBExp(e1);
        let is2 = compileAExp(e2);
        let is3 = compileAExp(e3);
            append(is1, Branch(is2, is3) :: Nil)
}

///
/// Compiles the given boolean expression to a sequence of instructions.
///
def compileBExp(e: BExp): List[Inst] = match e with {
    case True           => Push(1) :: Nil
    case False          => Push(0) :: Nil
    case Not(e)         =>
        let is = compileBExp(e);
            append(is, Neg :: Nil)
    case Conj(e1, e2)   =>
        let is1 = compileBExp(e1);
        let is2 = compileBExp(e2);
            append(append(is1, is2), And :: Nil)
    case Disj(e1, e2)   =>
        let is1 = compileBExp(e1);
        let is2 = compileBExp(e2);
            append(append(is1, is2), Or :: Nil)
    case Eq(e1, e2)     =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            append(append(is1, is2), Cmp :: Nil)
    case Neq(e1, e2)    =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            append(append(is1, is2), Neg :: Cmp :: Nil)
}

///
/// Here are some of the helper functions we used.
///
def append(xs: List[Inst], ys: List[Inst]): List[Inst] = match xs with {
    case Nil => ys
    case z :: zs => z :: append(zs, ys)
}

///
/// We test each compiler by writing some small test.
///
def testCompileAExp1: List[Inst] = compileAExp(Plus(Cst(1), Cst(2)))
