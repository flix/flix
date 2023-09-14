//
// In this tutorial we demonstrate the functional language in Flix.
//
// We write some interpreters and compilers for a small toy programming language.
//

//
// We define an enum to capture the syntax of arithmetic expressions.
//
enum AExp {
    // a literal integer constant.
    case Cst(Int32),

    // a binary addition expression: e1 + e2.
    case Plus(AExp, AExp),

    // a binary subtraction expression: e1 - e2.
    case Minus(AExp, AExp),

    // a binary multiplication expression: e1 * e2.
    case Times(AExp, AExp),

    //n a if-then-else expression: if (e1) e2 else e3.
    case IfThenElse(BExp, AExp, AExp)
}

//
// We then define an enum to capture the syntax of boolean expressions.
//
enum BExp {
    // the true boolean literal.
    case True,

    // the false boolean literal.
    case False,

    // a logical negation expression: !e.
    case Not(BExp),

    // a logical conjunction expression: e1 and e2.
    case Conj(BExp, BExp),

    // a logical disjunction expression: e1 or e2.
    case Disj(BExp, BExp),

    // an equality of expression: e1 == e2.
    case Eq(AExp, AExp),

    // an inequality of expression: e1 != e2.
    case Neq(AExp, AExp)
}

//
// We now define a small interpreter for arithmetic expressions.
//
def evalAExp(e: AExp): Int32 = match e {
    case Cst(i)                 => i
    case Plus(e1, e2)           => evalAExp(e1) + evalAExp(e2)
    case Minus(e1, e2)          => evalAExp(e1) - evalAExp(e2)
    case Times(e1, e2)          => evalAExp(e1) * evalAExp(e2)
    case IfThenElse(e1, e2, e3) =>
        let cond = evalBExp(e1);
            if (cond) evalAExp(e2) else evalAExp(e3)
}

//
// and here is the small interpreter for boolean expressions.
//
def evalBExp(e: BExp): Bool = match e {
    case True           => true
    case False          => false
    case Not(e1)        => not evalBExp(e1)
    case Conj(e1, e2)   => evalBExp(e1) and evalBExp(e2)
    case Disj(e1, e2)   => evalBExp(e1) or evalBExp(e2)
    case Eq(e1, e2)     => evalAExp(e1) == evalAExp(e2)
    case Neq(e1,e2)     => evalAExp(e1) != evalAExp(e2)
}

//
// We test each interpreter by writing some tests.
//
// You can run these functions by passing the `--main` argument to Flix, e.g.
//   $ flix <file> --main testEvalAExp1
//

@Test
def testEvalAExp1(): Int32 = evalAExp(Cst(42))

@Test
def testEvalAExp2(): Int32 = evalAExp(Plus(Cst(42), Cst(21)))

@Test
def testEvalAExp3(): Int32 = evalAExp(Minus(Cst(42), Cst(21)))

@Test
def testEvalAExp4(): Int32 = evalAExp(IfThenElse(True, Cst(1), Cst(2)))

@Test
def testEvalAExp5(): Int32 = evalAExp(IfThenElse(Neq(Cst(1), Cst(2)), Cst(42), Cst(21)))

@Test
def testEvalBExp1(): Bool = evalBExp(True)

@Test
def testEvalBExp2(): Bool = not evalBExp(Not(True))

@Test
def testEvalBExp3(): Bool = not evalBExp(Conj(True, False))

@Test
def testEvalBExp4(): Bool = evalBExp(Disj(True, False))

@Test
def testEvalBExp5(): Bool = evalBExp(Neq(Cst(1), Cst(2)))

//
// We now write two compilers that translate arithmetic and boolean expressions
// into a stack-based language.
//

//
// We define an enum to capture the syntax of instructions.
//
enum Inst {

    // an instruction that pushes the constant integer on the stack.
    case Push(Int32),

    // an instruction that adds the top two operands on the stack.
    case Add,

    // an instruction that substracts the top two operands on the stack.
    case Sub,

    // an instruction that multiplies the top two operands on the stack.
    case Mul,

    // an instruction that negates the top operand on the stack.
    case Neg,

    // an instruction that computes the logical-and of the top two operands on the stack.
    case And,

    // an instruction that computes the logical-or of the top two operands on the stack.
    case Or,

    // an instruction that computes equality of the top two operands on the stack.
    case Cmp,

    // an instruction that branches based on the top operand on the stack.
    case Branch(List[Inst], List[Inst])
}

//
// We then write a compiler from arithmetic expressions to a list of instructions:
//
// The append function appends two lists and is defined in the bottom of this file.
//
def compileAExp(e: AExp): List[Inst] = match e {
    case Cst(i)         => Push(i) :: Nil
    case Plus(e1, e2)   =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            is2 ::: is1 ::: Add :: Nil
    case Minus(e1, e2)  =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            is2 ::: is1 ::: Sub :: Nil
    case Times(e1, e2)  =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            is2 ::: is1 ::: Mul :: Nil
    case IfThenElse(e1, e2, e3)  =>
        let is1 = compileBExp(e1);
        let is2 = compileAExp(e2);
        let is3 = compileAExp(e3);
            is1 ::: Branch(is2, is3) :: Nil
}

//
// and a compiler for boolean expressions:
//
def compileBExp(e: BExp): List[Inst] = match e {
    case True           => Push(1) :: Nil
    case False          => Push(0) :: Nil
    case Not(e1)         =>
        let is = compileBExp(e1);
            is ::: Neg :: Nil
    case Conj(e1, e2)   =>
        let is1 = compileBExp(e1);
        let is2 = compileBExp(e2);
            is2 ::: is1 ::: And :: Nil
    case Disj(e1, e2)   =>
        let is1 = compileBExp(e1);
        let is2 = compileBExp(e2);
            is2 ::: is1 ::: Or :: Nil
    case Eq(e1, e2)     =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            is2 ::: is1 ::: Cmp :: Nil
    case Neq(e1, e2)    =>
        let is1 = compileAExp(e1);
        let is2 = compileAExp(e2);
            is2 ::: is1 ::: Neg :: Cmp :: Nil
}

//
// We test each compiler by writing some tests:
//
@Test
def testCompileAExp1(): List[Inst] = compileAExp(Cst(42))

@Test
def testCompileAExp2(): List[Inst] = compileAExp(Plus(Cst(42), Cst(21)))

@Test
def testCompileAExp3(): List[Inst] = compileAExp(Minus(Cst(42), Cst(21)))

@Test
def testCompileAExp4(): List[Inst] = compileAExp(IfThenElse(True, Cst(1), Cst(2)))

@Test
def testCompileAExp5(): List[Inst] = compileAExp(IfThenElse(Neq(Cst(1), Cst(2)), Cst(42), Cst(21)))

@Test
def testCompileBExp1(): List[Inst] = compileBExp(True)

@Test
def testCompileBExp2(): List[Inst] = compileBExp(Not(True))

@Test
def testCompileBExp3(): List[Inst] = compileBExp(Conj(True, False))

@Test
def testCompileBExp4(): List[Inst] = compileBExp(Disj(True, False))

@Test
def testCompileBExp5(): List[Inst] = compileBExp(Neq(Cst(1), Cst(2)))

//
// Finally, we write an interpreter for the stack-based language:
//
def evalInst(instructions: List[Inst], stack: List[Int32]): Int32 = match (instructions, stack) {
    case (Nil, x :: _) => x
    case ((Push(i)) :: rs, st) => evalInst(rs, i :: st)
    case (Add :: rs, i1 :: i2 :: st) => evalInst(rs, (i1 + i2) :: st)
    case (Sub :: rs, i1 :: i2 :: st) => evalInst(rs, (i1 - i2) :: st)
    case (Mul :: rs, i1 :: i2 :: st) => evalInst(rs, (i1 * i2) :: st)
    case (Neg :: rs, i :: st) =>
        if (i == 0)
            evalInst(rs, 1 :: st)
        else
            evalInst(rs, 0 :: st)
    case (And :: rs, i1 :: i2 :: st) =>
        if (i1 != 0 and i2 != 0)
            evalInst(rs, 1 :: st)
        else
            evalInst(rs, 0 :: st)
    case (Or :: rs, i1 :: i2 :: st) =>
        if (i1 != 0 or i2 != 0)
            evalInst(rs, 1 :: st)
        else
            evalInst(rs, 0 :: st)
    case (Cmp :: rs, i1 :: i2 :: st) =>
        if (i1 == i2)
            evalInst(rs, 1 :: st)
        else
            evalInst(rs, 0 :: st)
    case ((Branch(is1, is2)) :: _, i :: st) =>
        if (i != 0)
            evalInst(is1, st)
        else
            evalInst(is2, st)
    case _ => ??? // shouldn't happen
}

//
// and we can test the compiler and interpreter:
//
@Test
def testCompileAndEval1(): Int32 = evalInst(compileAExp(Cst(42)), Nil)

@Test
def testCompileAndEval2(): Int32 = evalInst(compileAExp(Plus(Cst(42), Cst(21))), Nil)

@Test
def testCompileAndEval3(): Int32 = evalInst(compileAExp(Minus(Cst(42), Cst(21))), Nil)

@Test
def testCompileAndEval4(): Int32 = evalInst(compileAExp(IfThenElse(True, Cst(1), Cst(2))), Nil)

@Test
def testCompileAndEval5(): Int32 = evalInst(compileAExp(IfThenElse(Neq(Cst(1), Cst(2)), Cst(42), Cst(21))), Nil)

@Test
def testCompileAndEval6(): Int32 = evalInst(compileAExp(IfThenElse(Eq(Cst(1), Cst(2)), Cst(42), Times(Cst(21), Cst(82)))), Nil)
