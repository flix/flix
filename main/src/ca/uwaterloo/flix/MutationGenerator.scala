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

import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.Type.{False, Str, True}
import ca.uwaterloo.flix.language.ast.{Ast, Name, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr

import java.math.BigInteger
import ca.uwaterloo.flix.language.ast.Type.AssocType
import ca.uwaterloo.flix.language.ast.Type.Alias
import ca.uwaterloo.flix.language.ast.Type.Cst
import ca.uwaterloo.flix.language.ast.Type.Var
import ca.uwaterloo.flix.language.ast.Type.Apply
import ca.uwaterloo.flix.language.ast.TypeConstructor.Intersection
import ca.uwaterloo.flix.language.ast.TypeConstructor.Union
import ca.uwaterloo.flix.language.ast.TypeConstructor.Schema
import ca.uwaterloo.flix.language.ast.TypeConstructor.RecordRowExtend
import ca.uwaterloo.flix.language.ast.TypeConstructor.Complement
import ca.uwaterloo.flix.language.ast.TypeConstructor.Pure
import ca.uwaterloo.flix.language.ast.TypeConstructor.Relation
import ca.uwaterloo.flix.language.ast.TypeConstructor.Univ
import ca.uwaterloo.flix.language.ast.TypeConstructor.Lazy
import ca.uwaterloo.flix.language.ast.TypeConstructor.Effect
import ca.uwaterloo.flix.language.ast.TypeConstructor.Receiver
import ca.uwaterloo.flix.language.ast.TypeConstructor.Not
import ca.uwaterloo.flix.language.ast.TypeConstructor.Int16
import ca.uwaterloo.flix.language.ast.TypeConstructor.SchemaRowExtend
import ca.uwaterloo.flix.language.ast.TypeConstructor.Native
import ca.uwaterloo.flix.language.ast.TypeConstructor.RecordRowEmpty
import ca.uwaterloo.flix.language.ast.TypeConstructor.Bool
import ca.uwaterloo.flix.language.ast.TypeConstructor.Float64
import ca.uwaterloo.flix.language.ast.TypeConstructor.Record
import ca.uwaterloo.flix.language.ast.TypeConstructor.CaseIntersection
import ca.uwaterloo.flix.language.ast.TypeConstructor.Int8
import ca.uwaterloo.flix.language.ast.TypeConstructor.Int64
import ca.uwaterloo.flix.language.ast.TypeConstructor.Float32
import ca.uwaterloo.flix.language.ast.TypeConstructor.RegionToStar
import ca.uwaterloo.flix.language.ast.TypeConstructor.Arrow
import ca.uwaterloo.flix.language.ast.TypeConstructor.And
import ca.uwaterloo.flix.language.ast.TypeConstructor.Ref
import ca.uwaterloo.flix.language.ast.TypeConstructor.CaseSet
import ca.uwaterloo.flix.language.ast.TypeConstructor.Lattice
import ca.uwaterloo.flix.language.ast.TypeConstructor.Int32
import ca.uwaterloo.flix.language.ast.TypeConstructor.Regex
import ca.uwaterloo.flix.language.ast.TypeConstructor.CaseComplement
import ca.uwaterloo.flix.language.ast.TypeConstructor.Tuple
import ca.uwaterloo.flix.language.ast.TypeConstructor.RestrictableEnum
import ca.uwaterloo.flix.language.ast.TypeConstructor.Sender
import ca.uwaterloo.flix.language.ast.TypeConstructor.CaseUnion
import ca.uwaterloo.flix.language.ast.TypeConstructor.SchemaRowEmpty
import ca.uwaterloo.flix.language.ast.TypeConstructor.Or


///
/// The Mutation Generator has a single purpose,
/// which is to generate mutations that MutationTester can evaluate
///
/// It works by moving through the root and definitions and creating every reasonable mutation.
/// This is returned as a list of mutated definitions.
///

object MutationGenerator {
  /**
    * Generates all possible mutants of the given module
    *
    * @param root             : The TypedAST
    * @param productionModule : The name of the module that is to be mutated
    * @return List[(Symbol.DefnSym, List[TypedAst.Def])]: the list of the symbols of the defs along with a
    *         list that contains a def for all mutations of that def
    */
  def mutateRoot(root: TypedAst.Root, productionModule: String): List[(Symbol.DefnSym, List[TypedAst.Def])] = {
    val defs = root.defs
    val defSyms = root.modules.filter(pair => pair._1.toString.equals(productionModule)).values.toList.flatten
    mutateDefs(defs, defSyms).flatten
  }


  /**
    *
    * @param defs    : a map of the all defs
    * @param defSyms : the symbols of the defs that are to be mutated
    * @return List[(Symbol.DefnSym, List[TypedAst.Def])]: the list of the symbols of the defs along with a
    *         list that contains a def for all mutations of that def
    */
  private def mutateDefs(defs: Map[Symbol.DefnSym, TypedAst.Def], defSyms: List[Symbol]) = {
    defs.toList.map(d => (d._1, d._2) match {
      case (s, fun) =>
        if (defSyms.contains(s)) {
          println(s, fun.exp)
          val mutExps = mutateExpr(fun.exp)
          val mutDefs = mutExps.map(mexp => {
            val mutatedDef = fun.copy(exp = mexp)
            MutationTester.insertDecAndCheckInDef(mutatedDef)
          })
          Some(d._1 -> mutDefs)
        } else None
      case _ => None
    })
  }


  def mutateSig(sig: Expr.Sig): List[TypedAst.Expr.Sig] = {
    val tpe = sig.tpe
    val sym = sig.sym
    val sub = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Sub"), Name.Ident(sym.loc.sp1, "sub", sym.loc.sp2)))
    val mul = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Mul"), Name.Ident(sym.loc.sp1, "mul", sym.loc.sp2)))
    val div = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Div"), Name.Ident(sym.loc.sp1, "div", sym.loc.sp2)))
    val add = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Add"), Name.Ident(sym.loc.sp1, "add", sym.loc.sp2)))
    val clazz = Symbol.mkClassSym("Order")
    val le = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "less", sym.loc.sp2)))
    val leq = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "lessEqual", sym.loc.sp2)))
    val gre = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "greater", sym.loc.sp2)))
    val greq = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "greaterEqual", sym.loc.sp2)))
    val compare = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "compare", sym.loc.sp2)))

    (sym.toString(), tpe.arrowArgTypes) match {
      case ("Add.add", List(Str, Str)) => Nil
      case ("Add.add", _) =>
        sub :: div :: mul :: Nil
      case ("Sub.sub", _) =>
        add :: div :: mul :: Nil
      case ("Div.div", _) =>
        mul :: add :: sub :: Nil
      case ("Mul.mul", _) =>
        div :: add :: sub :: Nil
      case ("Eq.eq", _) =>
        sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Eq"), Name.Ident(sym.loc.sp1, "neq", sym.loc.sp2))) :: Nil
      case ("Eq.neq", _) =>
        sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Eq"), Name.Ident(sym.loc.sp1, "eq", sym.loc.sp2))) :: Nil
      case ("Order.less", _) =>
        leq :: gre :: greq :: compare :: Nil
      case ("Order.lessEqual", _) =>
        le :: gre :: greq :: compare :: Nil
      case ("Order.greaterEqual", _) =>
        leq :: le :: gre :: compare :: Nil
      case ("Order.greater", _) =>
        leq :: le :: greq :: compare :: Nil
      case ("Order.compare", _) =>
        leq :: le :: greq :: gre :: Nil
      case _ => Nil
    }
  }

  /**
    * Goes through an expression and its subtree and returns a list of all possible mutations and permutations
    *
    * @param e : the expression to be mutated
    * @return List[TypedAst.Expr]: all possible mutants from the given expression and its subtree
    *
    *         For instance for an IfThenElse expression we generate three lists of mutations. The first includes the two mutants
    *         where the condition has been changed to either to true or false. The second list includes all generated mutants of the
    *         subtree of the then-branch, the same is done for the else-branch respectively.
    *
    *         Moreover for MatchCase expression we generate a mutant for each possible permutation of the cases and mutants
    *         where there in each mutant has been removed a case (expect the las of course). Furthermore we generate a list of
    *         mutants for each case, which includes all mutants of the respective subtree.
    */
  private def mutateExpr(e: TypedAst.Expr): List[TypedAst.Expr] = e match {
    case Expr.Cst(cst, tpe, loc) =>
      mutateCst(cst).map(c => Expr.Cst(c, tpe, loc))
    case exp@Expr.Var(_, _, _) =>
      mutateVar(exp)
    case Expr.Def(_, _, _) => Nil
    case original@Expr.Sig(_, _, _) =>
      mutateSig(original)
    case Expr.Hole(_, _, _) => Nil
    case Expr.HoleWithExp(_, _, _, _) => Nil
    case Expr.OpenAs(_, _, _, _) => Nil
    case original@Expr.Use(_, _, exp, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.Lambda(_, exp, _, _) =>
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

    case original@Expr.Binary(_, exp1, exp2, _, _, _) =>
      val mut2 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut3 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut2 ::: mut3
    case original@Expr.Let(_, _, exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut1 ::: mut2
    case original@Expr.LetRec(_, _, _, exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut1 ::: mut2
    case Expr.Region(_, _) => Nil
    case Expr.Scope(_, _, _, _, _, _) => Nil
    case original@Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val ifTrue = original.copy(exp1 = Expr.Cst(Constant.Bool(true), True, exp1.loc))
      val ifFalse = original.copy(exp1 = Expr.Cst(Constant.Bool(false), False, exp1.loc))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
      ifTrue :: ifFalse :: mut2 ::: mut3
    case original@Expr.Stm(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut1 ::: mut2
    case Expr.Discard(_, _, _) => Nil
    case original@Expr.Match(_, rules, _, _, _) =>
      val permutations = rules.permutations.toList.map(m => original.copy(rules = m))
      val deletedCasesMutation = rules.indices
        .map(index =>
          rules.filter(e => rules.indexOf(e) != index || rules.indexOf(e) == rules.length - 1))
        .toList.map(m => original.copy(rules = m))
      val mutateRules = rules.zipWithIndex.flatMap {
        case (rule, index) =>
          val mutations = mutateMatchrule(rule)
          mutations.map(m => original.copy(rules = rules.updated(index, m)))
      }
      val lengths = mutateRules.map(mr => mr.rules.length)
      lengths.foreach(l => assert(rules.length == l, "fail in match"))
      permutations ::: mutateRules ::: deletedCasesMutation.reverse.tail
    case Expr.TypeMatch(exp, rules, _, _, _) => Nil
    case Expr.RestrictableChoose(_, _, _, _, _, _) => Nil
    case Expr.Tag(_, exp, _, _, _) => Nil
    case Expr.RestrictableTag(_, _, _, _, _) => Nil
    case original@Expr.Tuple(elms, _, _, _) =>
      elms.zipWithIndex.flatMap {
        case (exp, index) =>
          val mutations = mutateExpr(exp)
          mutations.map(m => original.copy(elms = elms.updated(index, m)))
      }
    case Expr.RecordEmpty(_, _) => Nil
    case o@Expr.RecordSelect(exp, label, tpe, _, _) =>
      def nameAndType(xs: List[TypeConstructor]): List[(Name.Label, TypeConstructor)] = xs match {
        case RecordRowExtend(label) :: tp :: tail  => (label, tp) :: nameAndType(tail)
        case _ :: tail => nameAndType(tail)
        case _ => Nil
      }
      val types = nameAndType(exp.tpe.typeConstructors)
      val res = types.flatMap {
        case (name, tp) =>
          if (!name.equals(label) && tp.equals(tpe.typeConstructor.get))
            Some(o.copy(label = name))
          else None
      }
      println("mutation for record select")
      println(res)
      res
    case o@Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => o.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => o.copy(exp2 = m))
      mut1 ::: mut2
    case Expr.RecordRestrict(_, exp, _, _, _) => Nil
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
      Nil
    case original@Expr.InstanceOf(exp, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.CheckedCast(_, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.UncheckedCast(exp, _, _, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.UncheckedMaskingCast(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.Without(exp, _, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.TryCatch(exp, _, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.TryWith(exp, _, _, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.Do(_, exps, _, _, _) =>
      exps.zipWithIndex.flatMap {
        case (exp, index) =>
          val mutations = mutateExpr(exp)
          mutations.map(m => original.copy(exps = exps.updated(index, m)))
      }
    case original@Expr.InvokeConstructor(_, exps, _, _, _) =>
      val mutateExps = exps.map(e => mutateExpr(e))
      mutateExps.map(m => original.copy(exps = m))
    case original@Expr.InvokeMethod(_, exp, exps, _, _, _) =>
      val mut = mutateExpr(exp).map(m => original.copy(exp = m))
      val mutateExps = exps.map(e => mutateExpr(e))
      mutateExps.map(m => original.copy(exps = m)) ::: mut
    case original@Expr.InvokeStaticMethod(_, exps, _, _, _) =>
      val mutateExps = exps.map(e => mutateExpr(e))
      mutateExps.map(m => original.copy(exps = m))
    case original@Expr.GetField(_, exp, _, _, _) =>
      mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.PutField(_, exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut1 ::: mut2
    case Expr.GetStaticField(_, _, _, _) => Nil
    case original@Expr.PutStaticField(_, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case Expr.NewObject(_, _, _, _, _, _) => Nil
    case original@Expr.NewChannel(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut1 ::: mut2
    case original@Expr.GetChannel(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.PutChannel(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut1 ::: mut2
    case Expr.SelectChannel(_, _, _, _, _) => Nil
    case original@Expr.Spawn(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut1 ::: mut2
    case original@Expr.ParYield(_, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.Lazy(exp, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.Force(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case Expr.FixpointConstraintSet(_, _, _) => Nil
    case original@Expr.FixpointLambda(_, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
      val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
      mut1 ::: mut2
    case original@Expr.FixpointSolve(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.FixpointFilter(_, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.FixpointInject(exp, _, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case original@Expr.FixpointProject(_, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
    case Expr.Error(_, _, _) => Nil
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
    * Generates mutants where a variable is treated like the constant of the respective value
    * and is overwritten with sensible constants values of the respective type, including of by one values.
    *
    * @param varexp : The Var expression to be mutated
    * @return List of expressions.
    */
  private def mutateVar(varexp: Expr.Var): List[Expr] = varexp match {
    case Expr.Var(_, tpe, loc) =>
      val one = tpe match {
        case Type.Cst(tc, _) => tc match {
          case TypeConstructor.Char =>
            Expr.Cst(Constant.Char(1), tpe, loc) :: Nil
          case TypeConstructor.Float32 =>
            Expr.Cst(Constant.Float32(1), tpe, loc) :: Nil
          case TypeConstructor.Float64 =>
            Expr.Cst(Constant.Float64(1), tpe, loc) :: Nil
          case TypeConstructor.BigDecimal =>
            Expr.Cst(Constant.BigDecimal(java.math.BigDecimal.valueOf(1)), tpe, loc) :: Nil
          case TypeConstructor.Int8 =>
            Expr.Cst(Constant.Int8(1), tpe, loc) :: Nil
          case TypeConstructor.Int16 =>
            Expr.Cst(Constant.Int16(1), tpe, loc) :: Nil
          case TypeConstructor.Int32 =>
            Expr.Cst(Constant.Int32(1), tpe, loc) :: Nil
          case TypeConstructor.Int64 =>
            Expr.Cst(Constant.Int64(1), tpe, loc) :: Nil
          case TypeConstructor.BigInt =>
            Expr.Cst(Constant.BigInt(java.math.BigInteger.valueOf(1)), tpe, loc) :: Nil
          case _ => Nil
        }
        case _ => Nil
      }
      if (one == Nil) return mutateVarToConstantByType(tpe)
      val newtpe = Type.mkPureUncurriedArrow(tpe :: tpe :: Nil, tpe, loc)
      val sub = Expr.Sig(Symbol.mkSigSym(Symbol.mkClassSym("Sub"), Name.Ident(loc.sp1, "sub", loc.sp2)), newtpe, loc)
      val add = Expr.Sig(Symbol.mkSigSym(Symbol.mkClassSym("Add"), Name.Ident(loc.sp1, "add", loc.sp2)), newtpe, loc)
      val appSub = Expr.Apply(sub, varexp :: one, tpe, Type.Pure, loc)
      val appAdd = Expr.Apply(add, varexp :: one, tpe, Type.Pure, loc)
      val ret = appSub :: appAdd :: Nil
      ret ::: mutateVarToConstantByType(tpe)
    case _ => Nil
  }

  private def mutateVarToConstantByType(constType: Type): List[Expr.Cst] = constType match {
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Bool =>
        val mut = Constant.Bool(true)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Bool, loc))
      case TypeConstructor.Char =>
        val mut = Constant.Char(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Bool, loc))
      case TypeConstructor.Float32 =>
        val mut = Constant.Float32(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Float32, loc))
      case TypeConstructor.Float64 =>
        val mut = Constant.Float64(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Float64, loc))
      case TypeConstructor.BigDecimal =>
        val mut = Constant.BigDecimal(java.math.BigDecimal.ZERO)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.BigDecimal, loc))
      case TypeConstructor.Int8 =>
        val mut = Constant.Int8(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Int8, loc))
      case TypeConstructor.Int16 =>
        val mut = Constant.Int16(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Int16, loc))
      case TypeConstructor.Int32 =>
        val mut = Constant.Int32(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Int32, loc))
      case TypeConstructor.Int64 =>
        val mut = Constant.Int64(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Int64, loc))
      case TypeConstructor.BigInt =>
        val mut = Constant.BigInt(BigInteger.ZERO)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.BigInt, loc))
      case TypeConstructor.Str =>
        val mut = Constant.Str("")
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Str, loc))
      case TypeConstructor.Regex =>
        val mut = Constant.Regex(java.util.regex.Pattern.compile("b"))
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Regex, loc))
      case e => println(s"$e not implemented in mutateConstantByType"); Nil
    }
    case _ => Nil
  }

  /**
    * Matches the constant to the specific type and returns a list of mutants.
    *
    * @param cst : The constant that is to be mutated
    * @return A list of constants to replace cst with
    */
  private def mutateCst(cst: Ast.Constant): List[Ast.Constant] = {
    cst match {
      case Constant.Unit => Nil
      case Constant.Null => Nil
      case Constant.Bool(lit) => Constant.Bool(!lit) :: Nil
      case Constant.Char(lit) => Constant.Char((lit ^ Char.MaxValue).toChar) :: Nil
      case Constant.Float32(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Float.MinValue, Float.MaxValue, lit + 1, lit - 1).filter(i => lit != i)
        litCand.toList.map(i => Constant.Float32(i))
      case Constant.Float64(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Double.MaxValue, Double.MinValue, lit + 1, lit - 1).filter(i => lit != i)
        litCand.toList.map(i => Constant.Float64(i))
      case Constant.BigDecimal(lit) =>
        val litCand = Set(java.math.BigDecimal.ZERO,
          java.math.BigDecimal.valueOf(-1),
          java.math.BigDecimal.ONE,
          lit.subtract(java.math.BigDecimal.ONE),
          java.math.BigDecimal.valueOf(2),
          java.math.BigDecimal.valueOf(4),
          java.math.BigDecimal.valueOf(6),
          java.math.BigDecimal.valueOf(8),
          java.math.BigDecimal.valueOf(16),
          lit.add(java.math.BigDecimal.ONE),
          lit.subtract(java.math.BigDecimal.ONE)).filter(i => lit != i)
        litCand.toList.map(i => Constant.BigDecimal(i))
      case Constant.Int8(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Byte.MaxValue, Byte.MinValue, lit + 1, lit - 1).map(i => i.toByte).filter(i => lit != i)
        litCand.toList.map(i => Constant.Int8(i))
      case Constant.Int16(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, Short.MinValue, Short.MaxValue, lit + 1, lit - 1).map(i => i.toShort).filter(i => lit != i)
        litCand.toList.map(i => Constant.Int16(i))
      case Constant.Int32(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Int.MaxValue, Int.MinValue, lit + 1, lit - 1).filter(i => lit != i)
        litCand.toList.map(i => Constant.Int32(i))
      case Constant.Int64(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Long.MaxValue, Long.MinValue, lit + 1, lit - 1).filter(i => lit != i)
        litCand.toList.map(i => Constant.Int64(i))
      case Constant.BigInt(lit) =>
        val litCand = Set(java.math.BigInteger.ZERO,
          java.math.BigInteger.valueOf(-1),
          java.math.BigInteger.ONE,
          lit.subtract(java.math.BigInteger.ONE),
          java.math.BigInteger.TWO,
          java.math.BigInteger.valueOf(4),
          java.math.BigInteger.valueOf(6),
          java.math.BigInteger.valueOf(8),
          java.math.BigInteger.valueOf(16),
          lit.add(java.math.BigInteger.ONE),
          lit.subtract(java.math.BigInteger.ONE)).filter(i => lit != i)
        litCand.toList.map(i => Constant.BigInt(i))
      case Constant.Str(lit) => Constant.Str(lit + "\b") :: Nil
      case Constant.Regex(_) => Constant.Regex(java.util.regex.Pattern.compile(".*")) :: Nil
    }
  }

}
