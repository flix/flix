/*
 *  Copyright 2019 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expression, FormalParam, MatchRule, Pattern, Root, SelectChannelRule, TypeParam}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError.{UnusedEnumSym, UnusedEnumTag, UnusedFormalParam, UnusedTypeParam, UnusedVarSym}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

// TODO: DOC
object Redundancy extends Phase[TypedAst.Root, TypedAst.Root] {

  // TODO: Empty or empty?

  // TODO: Write test cases for each type of e.g. variable usage/introduction.

  val Empty: Validation[Used, RedundancyError] = Used.empty.toSuccess

  object Used {

    val empty: Used = Used(MultiMap.Empty, Set.empty, Set.empty, Set.empty, Set.empty)

    def of(sym: Symbol.EnumSym, tag: String): Used = empty.copy(enumSyms = MultiMap.Empty + (sym, tag))

    def of(sym: Symbol.DefnSym): Used = empty.copy(defSyms = Set(sym))

    def of(sym: Symbol.VarSym): Used = empty.copy(varSyms = Set(sym))

  }

  case class Used(enumSyms: MultiMap[Symbol.EnumSym, String], defSyms: Set[Symbol.DefnSym], varSyms: Set[Symbol.VarSym], relSyms: Set[Symbol.RelSym], latSyms: Set[Symbol.LatSym]) {
    // TODO: EnumSym
    // TODO: Cases

    def ++(that: Used): Used =
      if (this eq Used.empty) {
        that
      } else if (that eq Used.empty) {
        this
      } else {
        Used(
          this.enumSyms ++ that.enumSyms,
          this.defSyms ++ that.defSyms,
          this.varSyms ++ that.varSyms,
          this.relSyms ++ that.relSyms,
          this.latSyms ++ that.latSyms
        )
      }

    def -(sym: Symbol.VarSym): Used = copy(varSyms = varSyms - sym)
  }

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, RedundancyError] = flix.phase("Redundancy") {
    val defs = root.defs.map { case (_, v) => visitDef(v, root) }

    val usedVal = sequence(defs).map {
      case us => us.foldLeft(Used.empty)(_ ++ _)
    }

    for {
      used <- usedVal
      _ <- checkUnusedEnumSymbolsAndTags(used)(root)
    } yield root

  }

  private def visitDef(defn: TypedAst.Def, root: TypedAst.Root): Validation[Used, RedundancyError] = {
    for {
      u <- usedExp(defn.exp)
      _ <- checkUnusedFormalParameters(defn, u)
      _ <- checkUnusedTypeParameters(defn)
      _ <- constantFoldExp(defn.exp, Map.empty)
    } yield u
  }

  /**
    * Checks for unused enum symbols and tags.
    */
  private def checkUnusedEnumSymbolsAndTags(used: Used)(implicit root: Root): Validation[List[Unit], RedundancyError] =
    traverse(root.enums) {
      case (sym, decl) if decl.mod.isPublic =>
        // Enum is public. No usage requirements.
        ().toSuccess
      case (sym, decl) =>
        // Enum is non-public.
        // Lookup usage information for this specific enum.
        used.enumSyms.get(sym) match {
          case None =>
            // Case 1: Enum is never used.
            UnusedEnumSym(sym).toFailure
          case Some(usedTags) =>
            // Case 2: Enum is used and here are its used tags.
            // Check if there is any unused tag.
            decl.cases.values.find(caze => usedTags.contains(caze.tag.name)) match {
              case None => ().toSuccess
              case Some(caze) => UnusedEnumTag(sym, caze.tag).toFailure
            }
        }
    }

  private def checkUnusedFormalParameters(defn: Def, used: Used): Validation[List[Unit], RedundancyError] = {
    traverse(defn.fparams) {
      case FormalParam(sym, _, _, _) if unused(sym, used) => UnusedFormalParam(sym).toFailure
      case FormalParam(_, _, _, _) => ().toSuccess
    }
  }

  private def checkUnusedTypeParameters(defn: TypedAst.Def): Validation[List[Unit], RedundancyError] = {
    traverse(defn.tparams) {
      case TypeParam(name, tvar, _) if unused(tvar, defn.tpe.typeVars) => UnusedTypeParam(name, defn.sym).toFailure
      case TypeParam(_, _, _) => ().toSuccess
    }
  }

  /**
    * Returns symbols used in the given expression `e0`.
    */
  private def usedExp(e0: TypedAst.Expression): Validation[Used, RedundancyError] = e0 match {
    case Expression.Unit(_) => Empty

    case Expression.True(_) => Empty

    case Expression.False(_) => Empty

    case Expression.Char(_, _) => Empty

    case Expression.Float32(_, _) => Empty

    case Expression.Float64(_, _) => Empty

    case Expression.Int8(_, _) => Empty

    case Expression.Int16(_, _) => Empty

    case Expression.Int32(_, _) => Empty

    case Expression.Int64(_, _) => Empty

    case Expression.BigInt(_, _) => Empty

    case Expression.Str(_, _) => Empty

    case Expression.Wild(_, _, _) => Empty

    case Expression.Var(sym, _, _, _) => Used.of(sym).toSuccess

    case Expression.Def(sym, _, _, _) => Used.of(sym).toSuccess

    case Expression.Eff(sym, _, _, _) => Empty

    case Expression.Hole(sym, _, _, _) => Empty

    case Expression.Lambda(fparam, exp, _, _, _) =>
      flatMapN(usedExp(exp)) {
        case used if unused(fparam.sym, used) => UnusedFormalParam(fparam.sym).toFailure
        case used => used.toSuccess
      }

    case Expression.Apply(exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      mapN(us1, us2)(_ ++ _)

    case Expression.Unary(_, exp, _, _, _) => usedExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      mapN(us1, us2)(_ ++ _)

    case Expression.Let(sym, exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      flatMapN(us1, us2) {
        case (u1, u2) if unused(sym, u2) => UnusedVarSym(sym).toFailure
        case (u1, u2) => ((u1 ++ u2) - sym).toSuccess
      }

    case Expression.LetRec(sym, exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      flatMapN(us1, us2) {
        // TODO: Redundancy: How do we want to check let-rec?
        case (u1, u2) if unused(sym, u1) => UnusedVarSym(sym).toFailure
        case (u1, u2) if unused(sym, u2) => UnusedVarSym(sym).toFailure
        case (u1, u2) => ((u1 ++ u2) - sym).toSuccess
      }

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      val us3 = usedExp(exp3)
      mapN(us1, us2, us3)(_ ++ _ ++ _)

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      val us1 = usedExp(exp)
      mapN(us1, traverse(rules) {
        case MatchRule(pat, guard, body) =>
          // TODO: Need to deal with pattern and guard.
          usedExp(body)
      }) {
        case (u1, xs) => xs.reduce(_ ++ _) ++ u1
      }

    case Expression.Switch(rules, _, _, _) =>
      val rulesVal = traverse(rules) {
        case (cond, body) => mapN(usedExp(cond), usedExp(body))(_ ++ _)
      }
      mapN(rulesVal) {
        case rs => rs.foldLeft(Used.empty)(_ ++ _)
      }

    case Expression.Tag(sym, tag, exp, _, _, _) =>
      mapN(usedExp(exp)) {
        case used => used ++ Used.of(sym, tag)
      }

    case Expression.Tuple(elms, _, _, _) => usedExps(elms)

    case Expression.RecordEmpty(_, _, _) => Empty

    case Expression.RecordSelect(exp, _, _, _, _) => usedExp(exp)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      val us1 = usedExp(value)
      val us2 = usedExp(rest)
      mapN(us1, us2)(_ ++ _)

    case Expression.RecordRestrict(_, rest, _, _, _) => usedExp(rest)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      usedExps(elms)

    case Expression.ArrayNew(elm, len, _, _, _) =>
      val us1 = usedExp(elm)
      val us2 = usedExp(len)
      mapN(us1, us2)(_ ++ _)

    case Expression.ArrayLoad(base, index, _, _, _) =>
      val us1 = usedExp(base)
      val us2 = usedExp(index)
      mapN(us1, us2)(_ ++ _)

    case Expression.ArrayLength(base, _, _, _) => usedExp(base)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      val us1 = usedExp(base)
      val us2 = usedExp(index)
      val us3 = usedExp(elm)
      mapN(us1, us2, us3)(_ ++ _ ++ _)

    case Expression.ArraySlice(base, begin, end, _, _, _) =>
      val us1 = usedExp(base)
      val us2 = usedExp(begin)
      val us3 = usedExp(end)
      mapN(us1, us2, us3)(_ ++ _ ++ _)

    case Expression.VectorLit(elms, _, _, _) => usedExps(elms)

    case Expression.VectorNew(elm, len, _, _, _) => usedExp(elm)

    case Expression.VectorLoad(base, _, _, _, _) => usedExp(base)

    case Expression.VectorStore(base, _, elm, _, _, _) =>
      val us1 = usedExp(base)
      val us2 = usedExp(elm)
      mapN(us1, us2)(_ ++ _)

    case Expression.VectorLength(base, _, _, _) => usedExp(base)

    case Expression.VectorSlice(base, _, _, _, _, _) => usedExp(base)

    case Expression.Ref(exp, _, _, _) => usedExp(exp)

    case Expression.Deref(exp, _, _, _) => usedExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      mapN(us1, us2)(_ ++ _)

    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? // TODO

    case Expression.Existential(fparam, exp, _, _) =>
      flatMapN(usedExp(exp)) {
        case used if unused(fparam.sym, used) => UnusedFormalParam(fparam.sym).toFailure
        case used => used.toSuccess
      }

    case Expression.Universal(fparam, exp, _, _) =>
      flatMapN(usedExp(exp)) {
        case used if unused(fparam.sym, used) => UnusedFormalParam(fparam.sym).toFailure
        case used => used.toSuccess
      }

    case Expression.Ascribe(exp, _, _, _) => usedExp(exp)

    case Expression.Cast(exp, _, _, _) => usedExp(exp)

    case Expression.NativeConstructor(_, args, _, _, _) => usedExps(args)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? // TODO

    case Expression.NativeField(_, _, _, _) => Used.empty.toSuccess

    case Expression.NativeMethod(_, args, _, _, _) => usedExps(args)

    case Expression.NewChannel(exp, _, _, _) => usedExp(exp)

    case Expression.GetChannel(exp, _, _, _) => usedExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      mapN(us1, us2)(_ ++ _)

    case Expression.SelectChannel(rules, defaultOpt, _, _, _) =>
      val defaultVal = defaultOpt match {
        case None => Empty
        case Some(default) => usedExp(default)
      }

      val rulesVal = traverse(rules) {
        case SelectChannelRule(sym, chan, body) =>
          val chanVal = usedExp(chan)
          val bodyVal = usedExp(body)
          flatMapN(chanVal, bodyVal) {
            case (usedChan, usedBody) if unused(sym, usedBody) => UnusedVarSym(sym).toFailure
            case (usedChan, usedBody) => (usedChan ++ usedBody).toSuccess
          }
      }
      mapN(defaultVal, rulesVal) {
        case (defaultUsed, rulesUsed) => rulesUsed.foldLeft(defaultUsed)(_ ++ _)
      }

    case Expression.Spawn(exp, _, _, _) => usedExp(exp)

    case Expression.Sleep(exp, _, _, _) => usedExp(exp)

    case Expression.FixpointConstraint(c, _, _, _) => usedConstraint(c)

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      mapN(us1, us2)(_ ++ _)

    case Expression.FixpointSolve(exp, _, _, _) => usedExp(exp)

    case Expression.FixpointProject(_, exp, _, _, _) => usedExp(exp)

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      mapN(us1, us2)(_ ++ _)

    case Expression.UserError(_, _, _) => Empty
  }

  private def usedExps(es: List[TypedAst.Expression]): Validation[Used, RedundancyError] =
    mapN(traverse(es)(usedExp)) {
      case xs => xs.foldLeft(Used.empty)(_ ++ _)
    }


  private def usedConstraint(c: TypedAst.Constraint): Validation[Used, RedundancyError] = ??? // TODO

  sealed trait Value

  object Value {

    case object Bool extends Value

  }


  private def eval(e0: TypedAst.Expression): Map[Symbol.VarSym, AbsVal] = e0 match {
    case Expression.Binary(op, Expression.Var(sym, _, _, _), Expression.Int32(v, _), tpe, eff, loc) => op match {
      case BinaryOperator.Equal => Map(sym -> AbsVal.Range(v, v))
      case _ => Map.empty
    }
    case _ => Map.empty
  }

  private def compatible(m1: Map[Symbol.VarSym, Redundancy.AbsVal], m2: Map[Symbol.VarSym, Redundancy.AbsVal], loc: SourceLocation): Validation[Unit, RedundancyError] = {
    val commonKeys = m1.keys.filter(k2 => m2.contains(k2))

    commonKeys.headOption match {
      case None =>
        ().toSuccess
      case Some(key) =>
        val v1 = m1(key)
        val v2 = m2(key)

        ???
    }
  }


  // TODO: We could store an upper, lower bound, and equal. At least for integers.
  // TODO: For other types it would just be equal and not equal.
  // TODO: Might even be for entire expressions, like xs.length > 0, xs.length == 0, and not just variables.

  sealed trait AbsVal

  object AbsVal {

    case class Range(b: Int, e: Int) extends AbsVal

  }


  // TODO
  private def isPure(e: Expression): Boolean = ???

  // TODO: Report an error if all arguments are known to a function call that is pure. but a function could be helping by constructing some large structure.
  // TODO: Maybe we need a measure on physical code size?

  private def constantFoldExp(e0: TypedAst.Expression, env0: Map[Symbol.VarSym, Pattern]): Validation[Value, RedundancyError] = e0 match {

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      for {
        _ <- constantFoldExp(exp1, env0)
        _ <- constantFoldExp(exp2, env0)
      } yield Value.Bool

    case Expression.Match(exp, rules, tpe, eff, loc) => exp match {
      case Expression.Var(sym, _, _, _) =>
        val rs = traverse(rules) {
          case MatchRule(pat, guard, body) =>
            env0.get(sym) match {
              case None =>
                constantFoldExp(body, env0 + (sym -> pat))

              case Some(pat2) =>
                mapN(unify(pat, pat2)) {
                  case _ => constantFoldExp(body, env0 + (sym -> pat))
                }
            }
        }

        mapN(rs) {
          case _ => Value.Bool
        }
    }
    case _ => Value.Bool.toSuccess
  }

  // TODO: Code like f(x), and f(x) is redundant if both are pure... This is just common sub-expression elimination.

  // TODO: Need notion of stable expression which should be used instead of variable symbol.
  sealed trait StableExp

  object StableExp {

    // TODO: Should literals be considered stable?


    //
    //    case Expression.Unit(loc) => ??? // TODO
    //    case Expression.True(loc) => ??? // TODO
    //    case Expression.False(loc) => ??? // TODO
    //    case Expression.Char(lit, loc) => ??? // TODO
    //    case Expression.Float32(lit, loc) => ??? // TODO
    //    case Expression.Float64(lit, loc) => ??? // TODO
    //    case Expression.Int8(lit, loc) => ??? // TODO
    //    case Expression.Int16(lit, loc) => ??? // TODO
    //    case Expression.Int32(lit, loc) => ??? // TODO
    //    case Expression.Int64(lit, loc) => ??? // TODO
    //    case Expression.BigInt(lit, loc) => ??? // TODO
    //    case Expression.Str(lit, loc) => ??? // TODO


    case class Var(sym: Symbol.VarSym) extends StableExp

    case class Def(sym: Symbol.DefnSym) extends StableExp

    case class Apply(exp1: StableExp, exp2: StableExp) extends StableExp

    //    case Expression.Unary(op, exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) => ??? // TODO
    //
    //    case Expression.Match(exp, rules, tpe, eff, loc) => ??? // TODO
    //    case Expression.Switch(rules, tpe, eff, loc) => ??? // TODO
    //    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.Tuple(elms, tpe, eff, loc) => ??? // TODO
    //    case Expression.RecordEmpty(tpe, eff, loc) => ??? // TODO
    //    case Expression.RecordSelect(exp, label, tpe, eff, loc) => ??? // TODO
    //    case Expression.RecordExtend(label, value, rest, tpe, eff, loc) => ??? // TODO
    //    case Expression.RecordRestrict(label, rest, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayLit(elms, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayLength(base, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorLit(elms, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorNew(elm, len, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorLoad(base, index, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorLength(base, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => ??? // TODO
    //    case Expression.Ref(exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.Deref(exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ??? // TODO
    //    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? // TODO
    //    case Expression.NewChannel(exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.GetChannel(exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ??? // TODO
    //    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ??? // TODO
    //    case Expression.Spawn(exp, tpe, eff, loc) => ??? // TODO

  }


  private def unify(p1: Pattern, p2: Pattern): Validation[Unit, RedundancyError] = (p1, p2) match {
    case (Pattern.Tag(_, tag1, pat1, _, _), Pattern.Tag(_, tag2, pat2, _, _)) =>
      if (tag1 == tag2)
        unify(pat1, pat2)
      else
        RedundancyError.ImpossibleMatch(p1.loc, p2.loc).toFailure
    case _ => ().toSuccess
  }

  private def unused(sym: Symbol.VarSym, used: Redundancy.Used): Boolean =
    !used.varSyms.contains(sym) && sym.loc != SourceLocation.Unknown // TODO: Need better mechanism.

  private def unused(sym: Type.Var, used: Set[Type.Var]): Boolean = !used.contains(sym)

  object MultiMap {
    def Empty[K, V]: MultiMap[K, V] = MultiMap(Map.empty)
  }

  case class MultiMap[K, V](m: Map[K, Set[V]]) {

    def get(k: K): Option[Set[V]] = m.get(k)

    def +(k: K, v: V): MultiMap[K, V] = {
      val s = m.getOrElse(k, Set.empty)
      MultiMap(m + (k -> (s + v)))
    }

    def +(k: K, v: Set[V]): MultiMap[K, V] = {
      val s = m.getOrElse(k, Set.empty)
      MultiMap(m + (k -> (s ++ v)))
    }

    // TODO: Efficiency
    def ++(that: MultiMap[K, V]): MultiMap[K, V] = {
      val keys2 = that.m.keys
      keys2.foldLeft(this) {
        case (macc, k) => macc + (k, that.m(k))
      }
    }
  }

  // TODO: What counts as a use of an enum? Is it enough to (a) mention its type, (b) to use it in a pat match, or (c) to actually construct a value.
  // TODO: The pattern matching is difficult, because you could have a default match onsomething just of that type.
  // TODO: What about the void enum? How would you deal with that? What about the singleton. The above choices start to seem more false in the presence of those.
  // TODO: Add appropriate cases once a definition has been decided.
  // And if you just require it to be used in a type, should that type then be used?

}
