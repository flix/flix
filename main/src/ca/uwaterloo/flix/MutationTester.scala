/*
 * Copyright 2024 Lukas Schröder, Samuel Skovbakke & Alexander Sommer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.SemanticOp.{BoolOp, CharOp, Float32Op, Float64Op, Int16Op, Int32Op, Int64Op, Int8Op, StringOp}
import ca.uwaterloo.flix.language.ast.Type.{Apply, False, Int32, True, Str}
import ca.uwaterloo.flix.language.ast.{Ast, Name, SemanticOp, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr

object MutationTester {
    def run(flix: Flix, tester: String, testee: String): Unit = {
        val root = flix.check().unsafeGet
        val start = System.nanoTime()
        val root1 = mutateRoot(root, testee)
        val end = System.nanoTime() - start
        val timeSec = end.toFloat / 1_000_000_000.0
        println(s"time to generate mutations: $timeSec")
        runMutations(flix,tester, root, root1)

        /**
          * val result = root1.map(r => flix.codeGen(r).unsafeGet)
          * val tests = result.map(res => res.getTests)
          */
    }

    private def runMutations(flix: Flix,tester: String,  root: TypedAst.Root, mutatedDefs: List[(Symbol.DefnSym, List[TypedAst.Def])]): Unit = {
        val totalStartTime = System.nanoTime()
        var timeTemp = 0.0
        var survivorCount = 0
        var mutantCounter = 0
        mutatedDefs.foreach(mut => {
            val defName = mut._1.toString
            val mutationAmount = mut._2.length
            println(s"testing $defName with $mutationAmount mutations")
            val defs = root.defs
            mut._2.foreach(mDef => {
                mutantCounter += 1
                val start = System.nanoTime()
                val n = defs + (mut._1 -> mDef)
                //println(s"mutation: $mDef")
                val newRoot = root.copy(defs = n)
                val cRes = flix.codeGen(newRoot).unsafeGet
                val testsFromTester = cRes.getTests.filter{case (s, _) => s.toString.contains(tester)}.toList
                val testResults = testsFromTester.forall(c =>
                    try {
                        c._2.run() match {
                            case java.lang.Boolean.TRUE => true
                            case _ => false
                        }
                    } catch {
                        case _: Throwable => false
                    }
                )
                timeTemp += (System.nanoTime() - start).toFloat / 1_000_000_000
                if (testResults) {
                    survivorCount += 1
                    val sym = mDef.sym.toString
                    //println(s"mutation in $sym survived")
                }
            })
        })
        val totalEndTime = System.nanoTime() - totalStartTime
        println(s"there where $survivorCount surviving mutations, out of $mutantCounter mutations")
        val average = timeTemp / mutantCounter
        println(s"average time to test a mutant:  $average sec")
        val time = totalEndTime.toFloat / 1_000_000_000
        println(s"total time to test all mutants: $time sec")
    }

    private def mutateRoot(root: TypedAst.Root, testee: String): List[(Symbol.DefnSym, List[TypedAst.Def])] = {
        val defs = root.defs
        val defSyms = root.modules.filter(pair => (pair._1.toString.equals(testee))).values.toList.flatten
        mutateDefs(defs, defSyms).flatten
    }


    private def mutateDefs(defs: Map[Symbol.DefnSym, TypedAst.Def], defSyms: List[Symbol]) = {
        defs.toList.map(d => (d._1, d._2) match {
            case (s, fun) =>
                if (defSyms.contains(s)) {
                    val mutExps = mutateExpr(fun.exp)
                    val mutDefs = mutExps.map(mexp => fun.copy(exp = mexp))
                    Some(d._1 -> mutDefs)
                } else None
            case _ => None
        })
    }


  def mutateSig(sig: Expr.Sig): List[TypedAst.Expr.Sig]= {
    val tpe = sig.tpe
    val sym = sig.sym

    (sym.toString(), tpe.arrowArgTypes) match {
      case ("Add.add", List(Str, Str)) => Nil
      case ("Add.add", _) | ("Sub.sub", _) | ("Div.div", _)| ("Mul.mul", _) =>
        val sub = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Sub"), Name.Ident(sym.loc.sp1, "sub", sym.loc.sp2)))
        val add = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Add"), Name.Ident(sym.loc.sp1, "add", sym.loc.sp2)))
        val div = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Div"), Name.Ident(sym.loc.sp1, "div", sym.loc.sp2)))
        val mul = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Mul"), Name.Ident(sym.loc.sp1, "mul", sym.loc.sp2)))
        sub :: add :: div :: mul :: Nil
      case ("Eq.eq", _) =>
        sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Eq"),  Name.Ident(sym.loc.sp1, "neq", sym.loc.sp2))) :: Nil
      case ("Eq.neq", _) =>
        sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Eq"), Name.Ident(sym.loc.sp1, "eq", sym.loc.sp2))) :: Nil
      case ("Order.less", _) | ("Order.lessEqual", _) | ("Order.greaterEqual", _) | ("Order.greater", _) | ("Order.compare", _) =>
        val clazz = Symbol.mkClassSym("Order")
        val le = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "less", sym.loc.sp2)))
        val leq = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "lessEqual", sym.loc.sp2)))
        val gre = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "greater", sym.loc.sp2)))
        val greq = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "greaterEqual", sym.loc.sp2)))
        val compare = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "compare", sym.loc.sp2)))
        le :: leq :: gre :: greq :: compare :: Nil

      case _ => Nil
    }

  }

  /**
      * // var doesn't contain a subtree, and we don't mutate them so we don't need to return them
      * case Expr.Var(_, _, _) => Nil
      */
    private def mutateExpr(e: TypedAst.Expr): List[TypedAst.Expr] = e match {
        case Expr.Cst(cst, tpe, loc) =>
            mutateCst(cst).map(m => Expr.Cst(m, tpe, loc))
        case original@Expr.Var(_, _, _) =>  Nil
        case original@Expr.Def(sym, _, _) => Nil
        case original@Expr.Sig(sym, tpe, loc) =>
          mutateSig(original).filter(e => e != original)
        case original@Expr.Hole(sym, _, _) => Nil
        case original@Expr.HoleWithExp(exp, _, _, _) => Nil
        case original@Expr.OpenAs(symUse, exp, _, _) => Nil
        case original@Expr.Use(sym, alias, exp, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Lambda(fparam, exp, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))

        case original@Expr.Apply(exp, exps, _, _, _) =>
            val mut = mutateExpr(exp).map(m => original.copy(exp = m))
            val mutateExps = exps.zipWithIndex.flatMap {
                case (exp, index) =>
                    val mutations = mutateExpr(exp)
                    mutations.map(m => original.copy(exps = exps.updated(index, m)))
            }
            val lengths = mutateExps.map(mr => mr.exps.length)
            lengths.foreach(l => assert(exps.length == l, "fail in apply"))
            mut ::: mutateExps

        case original@Expr.Unary(sop, exp, tpe, eff, loc) =>
            val mut1 = Expr.Unary(sop, original, tpe, eff, loc)
            mut1 :: mutateExpr(exp).map(m => original.copy(exp = m))

        case original@Expr.Binary(sop, exp1, exp2, _, _, _) =>
            val mut1 = mutateSop(sop).map(m => original.copy(sop = m))
            val mut2 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut3 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2 ::: mut3
        case original@Expr.Let(sym, mod, exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.LetRec(sym, ann, mod, exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.Region(tpe, loc) =>  Nil
        case original@Expr.Scope(sym, regionVar, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.IfThenElse(exp1, exp2, exp3, _, _, loc) =>
            val ifTrue = original.copy(exp1 = Expr.Cst(Constant.Bool(true), True, exp1.loc))
            val ifFalse = original.copy(exp1 = Expr.Cst(Constant.Bool(false), False, exp1.loc))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            ifTrue :: ifFalse :: mut2 ::: mut3
        case original@Expr.Stm(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.Discard(exp, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Match(_, rules, _, _, _) =>
            // refactor to permutation
            val permutations = rules.permutations.toList.map(m => original.copy(rules = m))
            val deletedCasesMutation = rules.indices
                .map(index =>
                    rules.filter(e => rules.indexOf(e) != index || rules.indexOf(e) == rules.length - 1))
                .toList.map(m => original.copy(rules = m))
            val mutateRules = rules.zipWithIndex.flatMap { case (rule, index) => {
                    val mutations = mutateMatchrule(rule)
                    mutations.map(m => original.copy(rules = rules.updated(index, m)))
                }
            }
            val lengths = mutateRules.map(mr => mr.rules.length)
            lengths.foreach(l => assert(rules.length == l, "fail in match"))
            permutations ::: mutateRules ::: deletedCasesMutation.reverse.tail
        case original@Expr.TypeMatch(exp, rules, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.RestrictableChoose(star, exp, rules, _, _, _) => Nil
        case original@Expr.Tag(sym, exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.RestrictableTag(sym, exp, _, _, _) => Nil
        case original@Expr.Tuple(elms, _, _, _) =>
            elms.zipWithIndex.flatMap {
                case (exp, index) =>
                    val mutations = mutateExpr(exp)
                    mutations.map(m => original.copy(elms = elms.updated(index, m)))
            }
        case original@Expr.RecordEmpty(tpe, loc) => Nil
        case original@Expr.RecordSelect(exp, label, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.RecordExtend(label, exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.RecordRestrict(label, exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.ArrayLit(exps, exp, _, _, _) =>
            val mut = mutateExpr(exp).map(m => original.copy(exp = m))
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m)) ::: mut
        case original@Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            mut1 ::: mut2 ::: mut3
        case original@Expr.ArrayLoad(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.ArrayLength(exp, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            mut1 ::: mut2 ::: mut3
        case original@Expr.VectorLit(exps, _, _, _) =>
            exps.zipWithIndex.flatMap {
                case (exp, index) =>
                    val mutations = mutateExpr(exp)
                    mutations.map(m => original.copy(exps = exps.updated(index, m)))
            }
        case original@Expr.VectorLoad(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.VectorLength(exp, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Ref(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.Deref(exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Assign(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.Ascribe(exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.InstanceOf(exp, clazz, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.CheckedCast(cast, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.UncheckedCast(exp, declaredType, declaredEff, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.UncheckedMaskingCast(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Without(exp, effUse, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.TryCatch(exp, rules, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.TryWith(exp, effUse, rules, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Do(op, exps, _, _, _) =>
            exps.zipWithIndex.flatMap {
                case (exp, index) =>
                    val mutations = mutateExpr(exp)
                    mutations.map(m => original.copy(exps = exps.updated(index, m)))
            }
        case original@Expr.InvokeConstructor(constructor, exps, _, _, _) =>
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m))
        case original@Expr.InvokeMethod(method, exp, exps, _, _, _) =>
            val mut = mutateExpr(exp).map(m => original.copy(exp = m))
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m)) ::: mut
        case original@Expr.InvokeStaticMethod(method, exps, _, _, _) =>
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m))
        case original@Expr.GetField(field, exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.PutField(field, exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.GetStaticField(field, _, _, _) =>  Nil
        case original@Expr.PutStaticField(field, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.NewObject(name, clazz, _, _, methods, _) =>  Nil
        case original@Expr.NewChannel(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.GetChannel(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.PutChannel(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.SelectChannel(rules, default, _, _, _) =>  Nil
        case original@Expr.Spawn(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.ParYield(frags, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Lazy(exp, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Force(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointConstraintSet(cs, _, _) =>  Nil
        case original@Expr.FixpointLambda(pparams, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointMerge(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.FixpointSolve(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointFilter(pred, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointInject(exp, pred, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointProject(pred, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Error(m, _, _) => Nil
    }

    private def mutateMatchrule(mr: TypedAst.MatchRule): List[TypedAst.MatchRule] = {
        val mut1 = mutateExpr(mr.exp).map(m => mr.copy(exp = m))
        val patterns = mutatePattern(mr.pat).map(m => mr.copy(pat = m))
        patterns ::: mut1
    }

    private def mutatePattern(pattern: TypedAst.Pattern): List[TypedAst.Pattern] = {
        pattern match {
            case original@TypedAst.Pattern.Cst(cst, _, _) => mutateCst(cst).map(m => original.copy(m))
            case _ => Nil
        }
    }

    /**
      * private def mutateSig(sig: Expr.Sig): List[Expr.Sig] = (sig.sym, sig.tpe) match {
      * case ("Add.add", t) => t match {
      * case Apply()
      * }
      *
      * }
      */
    private def mutateSop(sop: SemanticOp): List[SemanticOp] = {
        def helper(sop: SemanticOp): List[SemanticOp] = sop match {
            case op: SemanticOp.BoolOp => op match {
                case BoolOp.Not => BoolOp.Not :: Nil
                case _ => BoolOp.Or :: BoolOp.And :: BoolOp.Neq :: BoolOp.Eq :: Nil
            }
            case op: SemanticOp.CharOp => op match {
                case _ => CharOp.Neq :: CharOp.Eq :: CharOp.Neq :: CharOp.Ge :: CharOp.Le :: CharOp.Lt :: CharOp.Gt :: Nil
            }
            case op: SemanticOp.Float32Op => op match {
                case Float32Op.Neg => Float32Op.Neg :: Nil
                case Float32Op.Add | Float32Op.Sub | Float32Op.Mul |
                     Float32Op.Sub | Float32Op.Div | Float32Op.Exp =>
                    Float32Op.Add :: Float32Op.Sub :: Float32Op.Mul :: Float32Op.Sub :: Float32Op.Div :: Float32Op.Exp :: Nil
                case _ => Float32Op.Eq :: Float32Op.Neq :: Float32Op.Lt :: Float32Op.Le :: Float32Op.Gt :: Float32Op.Ge :: Nil
            }
            case op: SemanticOp.Float64Op => op match {
                case Float64Op.Neg | Float64Op.Add | Float64Op.Sub | Float64Op.Mul | Float64Op.Div | Float64Op.Exp =>
                    Float64Op.Neq :: Float64Op.Add :: Float64Op.Sub :: Float64Op.Mul :: Float64Op.Div :: Float64Op.Exp :: Nil
                case _ => Float64Op.Eq :: Float64Op.Neq :: Float64Op.Lt :: Float64Op.Le :: Float64Op.Gt :: Float64Op.Ge :: Nil
            }
            case op: SemanticOp.Int8Op => op match {
                case Int8Op.Neg => Int8Op.Neg :: Nil
                case Int8Op.Not => Int8Op.Not :: Nil
                case Int8Op.Add | Int8Op.Div | Int8Op.Sub | Int8Op.Mul | Int8Op.Rem | Int8Op.Exp | Int8Op.Shl | Int8Op.Shr =>
                    Int8Op.Add :: Int8Op.Div :: Int8Op.Sub :: Int8Op.Mul :: Int8Op.Rem :: Int8Op.Exp :: Int8Op.Shl :: Int8Op.Shr :: Nil
                case Int8Op.And | Int8Op.Or | Int8Op.Xor => Int8Op.And :: Int8Op.Or :: Int8Op.Xor :: Nil
                case _ => Int8Op.Eq :: Int8Op.Neq :: Int8Op.Lt :: Int8Op.Le :: Int8Op.Gt :: Int8Op.Ge :: Nil
            }
            case op: SemanticOp.Int16Op => op match {
                case Int16Op.Neg => Int16Op.Neg :: Nil
                case Int16Op.Not => Int16Op.Not :: Nil
                case Int16Op.Add | Int16Op.Sub | Int16Op.Mul | Int16Op.Div | Int16Op.Rem | Int16Op.Exp | Int16Op.Shr | Int16Op.Shl =>
                    Int16Op.Sub :: Int16Op.Sub :: Int16Op.Mul :: Int16Op.Div :: Int16Op.Rem :: Int16Op.Exp :: Int16Op.Shr :: Int16Op.Shl :: Nil
                case Int16Op.And | Int16Op.Or | Int16Op.Xor => Int16Op.And :: Int16Op.Or :: Int16Op.Xor :: Nil
                case _ => Int16Op.Eq :: Int16Op.Neq :: Int16Op.Lt :: Int16Op.Le :: Int16Op.Gt :: Int16Op.Ge :: Nil
            }
            case op: SemanticOp.Int32Op => op match {
                case Int32Op.Neg => Int32Op.Neg :: Nil
                case Int32Op.Not => Int32Op.Not :: Nil
                case Int32Op.Add | Int32Op.Sub | Int32Op.Mul | Int32Op.Div | Int32Op.Rem | Int32Op.Exp | Int32Op.Shr | Int32Op.Shl =>
                    Int32Op.Add :: Int32Op.Sub :: Int32Op.Mul :: Int32Op.Div :: Int32Op.Rem :: Int32Op.Exp :: Int32Op.Shr :: Int32Op.Shl :: Nil
                case Int32Op.And | Int32Op.Or | Int32Op.Xor => Int32Op.And :: Int32Op.Or :: Int32Op.Xor :: Nil
                case _ => Int32Op.Eq :: Int32Op.Neq :: Int32Op.Lt :: Int32Op.Le :: Int32Op.Gt :: Int32Op.Ge :: Nil
            }
            case op: SemanticOp.Int64Op => op match {
                case Int64Op.Neg => Int64Op.Neg :: Nil
                case Int64Op.Not => Int64Op.Not :: Nil
                case Int64Op.Add | Int64Op.Sub | Int64Op.Shl | Int64Op.Shr | Int64Op.Mul | Int64Op.Div | Int64Op.Rem | Int64Op.Exp =>
                    Int64Op.Add :: Int64Op.Sub :: Int64Op.Mul :: Int64Op.Shl :: Int64Op.Shr :: Int64Op.Div :: Int64Op.Rem :: Int64Op.Exp :: Nil
                case Int64Op.And | Int64Op.Or | Int64Op.Xor => Int64Op.And :: Int64Op.Or :: Int64Op.Xor :: Nil
                case _ => Int64Op.Eq :: Int64Op.Neq :: Int64Op.Lt :: Int64Op.Le :: Int64Op.Gt :: Int64Op.Ge :: Nil
            }
            case op: SemanticOp.StringOp => op match {
                case StringOp.Concat => StringOp.Concat :: Nil
            }
        }
        helper(sop).filter(e => e != sop)
    }

    private def mutateCst(cst: Ast.Constant): List[Ast.Constant] = {
        def helper(cst: Ast.Constant): List[Ast.Constant] = {
            cst match {
                case Constant.Unit => Nil
                case Constant.Null => Nil
                case Constant.Bool(lit) => Constant.Bool(!lit) :: Nil
                case Constant.Char(lit) => Constant.Char((lit ^ Char.MaxValue).toChar) :: Nil
                case original@Constant.Float32(lit) =>
                  Constant.Float32(lit + 1) :: Constant.Float32(lit - 1) :: Constant.Float32(1) ::
                        Constant.Float32(-1) :: Constant.Float32(0) :: Constant.Float32(2) :: Constant.Float32(4) :: Constant.Float32(8) ::
                        Constant.Float32(16) :: Constant.Float32(Float.MaxValue) :: Constant.Float32(Float.MinValue) :: Nil

                case Constant.Float64(lit) =>
                    Constant.Float64(lit + 1) :: Constant.Float64(lit - 1) ::
                        Constant.Float64(1) :: Constant.Float64(-1) :: Constant.Float64(0) ::
                        Constant.Float64(2) :: Constant.Float64(4) :: Constant.Float64(8) :: Constant.Float64(16) ::
                        Constant.Float64(Double.MaxValue) :: Constant.Float64(Double.MinValue) :: Nil
                case Constant.BigDecimal(lit) =>
                    Constant.BigDecimal(lit.add(java.math.BigDecimal.ONE)) :: Constant.BigDecimal(java.math.BigDecimal.ONE) ::
                        Constant.BigDecimal(lit.subtract(java.math.BigDecimal.ONE)) :: Constant.BigDecimal(java.math.BigDecimal.ZERO) ::
                        Constant.BigDecimal(java.math.BigDecimal.ZERO.subtract(java.math.BigDecimal.ONE)) :: Nil
                case Constant.Int8(lit) =>
                    (Constant.Int8(lit.+(1).toByte) :: Constant.Int8(lit.-(1).toByte)
                        :: Constant.Int8((-1).toByte) :: Constant.Int8((0.toByte))
                        :: Constant.Int8((1).toByte) :: Constant.Int8((2).toByte)
                        :: Constant.Int8((4).toByte) :: Constant.Int8((8).toByte) :: Constant.Int8((16).toByte)
                        :: Constant.Int8(Byte.MaxValue) :: Constant.Int8(Byte.MinValue) :: Nil)
                case Constant.Int16(lit) =>
                    (Constant.Int16(lit.+(1).toShort) :: Constant.Int16(lit.+(1).toShort)
                        :: Constant.Int16((-1).toShort) :: Constant.Int16((0).toShort)
                        :: Constant.Int16((1).toShort) :: Constant.Int16((2).toByte)
                        :: Constant.Int16((4).toShort) :: Constant.Int16((8).toShort) :: Constant.Int16((16).toShort)
                        :: Constant.Int16(Short.MaxValue) :: Constant.Int16(Short.MinValue) :: Nil)
                case Constant.Int32(lit) =>
                    (Constant.Int32(lit + 1) :: Constant.Int32(lit - 1)
                        :: Constant.Int32(-1) :: Constant.Int32(0) :: Constant.Int32(1)
                        :: Constant.Int32(2) :: Constant.Int32(4) :: Constant.Int32(8) :: Constant.Int32(16)
                        :: Constant.Int32(Int.MaxValue) :: Constant.Int32(Int.MinValue) :: Nil)
                case Constant.Int64(lit) =>
                    (Constant.Int64(lit + 1) :: Constant.Int64(lit - 1)
                        :: Constant.Int64(-1) :: Constant.Int64(0)
                        :: Constant.Int64(2) :: Constant.Int64(4) :: Constant.Int64(8) :: Constant.Int64(16)
                        :: Constant.Int64(1) :: Constant.Int64(Long.MaxValue)
                        :: Constant.Int64(Long.MinValue) :: Nil)
                case Constant.BigInt(lit) => Constant.BigInt(lit.add(lit)) :: Nil
                case Constant.Str(lit) => Constant.Str(lit + "\b") :: Nil
                case Constant.Regex(_) => Constant.Regex(java.util.regex.Pattern.compile("a")) :: Nil
            }
        }
        helper(cst).filter(c => c != cst)
    }
}
