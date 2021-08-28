/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Name, ResolvedAst, SemanticOperator, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
  * Constructs instances derived from enums.
  *
  * No errors are thrown in this phase: constructed instances must be well-formed.
  * Errors with overlapping instances or unfulfilled type constraints must be caught in later phases.
  */
object Deriver extends Phase[ResolvedAst.Root, ResolvedAst.Root] {

  // TODO copy-pasted from Typer
  // TODO we need a more universal lookup system

  /**
    * The following classes are assumed to always exist.
    *
    * Anything added here must be mentioned in `CoreLibrary` in the Flix class.
    */
  object PredefinedClasses {

    /**
      * Returns the class symbol with the given `name`.
      */
    def lookupClassSym(name: String, root: ResolvedAst.Root): Symbol.ClassSym = {
      val key = new Symbol.ClassSym(Nil, name, SourceLocation.Unknown)
      root.classes.get(key) match {
        case None => throw InternalCompilerException(s"The type class: '$key' is not defined.")
        case Some(clazz) => clazz.sym
      }
    }

    /**
      * Returns the sig symbol with the given `clazz` and name `sig`.
      */
    def lookupSigSym(clazz: String, sig: String, root: ResolvedAst.Root): Symbol.SigSym= {
      val clazzKey = new Symbol.ClassSym(Nil, clazz, SourceLocation.Unknown)
      val sigKey = new Symbol.SigSym(clazzKey, sig, SourceLocation.Unknown)
      root.classes(clazzKey).sigs(sigKey).sym
    }

  }

  override def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Root, Nothing] = {
    val derivedInstances = root.enums.values.flatMap {
      enum => getDerivations(enum, root)
    }

    val newInstances = derivedInstances.foldLeft(root.instances) {
      case (acc, inst) =>
        val accInsts = acc.getOrElse(inst.sym, Nil)
        acc + (inst.sym -> (inst :: accInsts))
    }

    root.copy(instances = newInstances).toSuccess
  }

  /**
    * Builds the instances derived from this enum.
    */
  def getDerivations(enum: ResolvedAst.Enum, root: ResolvedAst.Root)(implicit flix: Flix): List[ResolvedAst.Instance] = enum match {
    case ResolvedAst.Enum(_, _, _, _, derives, _, _, _, _) =>
      derives.map {
        case ResolvedAst.Derivation(sym, loc) if sym == PredefinedClasses.lookupClassSym("ToString", root) => createToString(enum, loc, root)
        case unknownSym => throw InternalCompilerException(s"Unexpected derivation: $unknownSym")
      }
  }

  /**
    * Creates a toString instance for the given enum.
    *
    * {{{
    * enum E[a] with ToString {
    *   case C1
    *   case C(a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance ToString[E[a]] with ToString[a] {
    *   pub def toString(x: E[a]): String = x match {
    *     case C1 => "C1"
    *     case C(y) => "C" + "(" + ToString.toString(y) + ")"
    *   }
    * }}}
    */
  def createToString(enum: ResolvedAst.Enum, loc: SourceLocation, root: ResolvedAst.Root)(implicit flix: Flix): ResolvedAst.Instance = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, _, cases, _, sc, _) =>
      // create a match rule for each case and put them in a match expression
      val matchRules = cases.values.map(createToStringMatchRule(_, loc, root))
      val varSym = Symbol.freshVarSym()
      val exp = ResolvedAst.Expression.Match(
        ResolvedAst.Expression.Var(varSym, varSym.tvar, loc),
        matchRules.toList,
        loc
      )

      val spec = ResolvedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(ResolvedAst.FormalParam(varSym, Ast.Modifiers.Empty, sc.base, loc)),
        sc = ResolvedAst.Scheme(
          tparams.tparams.map(_.tpe),
          List(ResolvedAst.TypeConstraint(PredefinedClasses.lookupClassSym("ToString", root), sc.base, loc)),
          Type.mkPureArrow(sc.base, Type.mkString(loc))
        ),
        tpe = Type.mkString(loc),
        eff = Type.Cst(TypeConstructor.True, loc),
        loc = loc
      )
      val defn = ResolvedAst.Def(Symbol.mkDefnSym("ToString.toString"), spec, exp)

      // Add a type constraint to the instance for any variable used in a case
      val caseTvars = for {
        caze <- cases.values
        tpe <- getTagArguments(caze.sc.base)
        if tpe.isInstanceOf[Type.Var]
      } yield tpe
      val tconstrs = caseTvars.toList.distinct.map(ResolvedAst.TypeConstraint(PredefinedClasses.lookupClassSym("ToString", root), _, loc))

      ResolvedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        mod = Ast.Modifiers.Empty,
        sym = PredefinedClasses.lookupClassSym("ToString", root),
        tpe = sc.base,
        tconstrs = tconstrs,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      )
  }

  /**
    * Creates a ToString match rule for the given enum case.
    */
  def createToStringMatchRule(caze: ResolvedAst.Case, loc: SourceLocation, root: ResolvedAst.Root)(implicit flix: Flix): ResolvedAst.MatchRule = caze match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated, sc) =>

      // get a pattern corresponding to this case
      val (pat, varSyms) = mkPattern(sc.base)

      val guard = ResolvedAst.Expression.True(loc)

      val tagPart = ResolvedAst.Expression.Str(tag.name, loc)

      // call toString on each variable
      val toStrings = varSyms.map {
        varSym =>
          ResolvedAst.Expression.Apply(
            ResolvedAst.Expression.Sig(PredefinedClasses.lookupSigSym("ToString", "toString", root), loc),
            List(ResolvedAst.Expression.Var(varSym, varSym.tvar, loc)),
            loc
          )
      }

      // put commas between the arguments
      val sep = mkExpr(", ", loc)
      val valuePart = intersperse(toStrings, sep)

      // put it all together
      val exp = valuePart match {
        // Case 1: no arguments: just show the tag
        case Nil => tagPart
        // Case 2: at least one argument: concatenate the tag with the values wrapped in parens
        case exps => concatAll(tagPart :: mkExpr("(", loc) :: (exps :+ mkExpr(")", loc)), loc)
      }

      ResolvedAst.MatchRule(pat, guard, exp)
  }

  /**
    * Builds a string expression from the given string.
    */
  def mkExpr(str: String, loc: SourceLocation): ResolvedAst.Expression.Str = ResolvedAst.Expression.Str(str, loc)

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  def concat(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, loc: SourceLocation): ResolvedAst.Expression = {
    ResolvedAst.Expression.Binary(SemanticOperator.StringOp.Concat, exp1, exp2, loc)
  }

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  def concatAll(exps: List[ResolvedAst.Expression], loc: SourceLocation): ResolvedAst.Expression = {
    exps match {
      case Nil => ResolvedAst.Expression.Str("", loc)
      case head :: tail => tail.foldLeft(head)(concat(_, _, loc))
    }
  }

  /**
    * Extracts the types from the given tag type.
    */
  def getTagArguments(tpe: Type): List[Type] = {
    tpe.typeArguments.headOption match {
      case None => throw InternalCompilerException("Unexpected empty type arguments.")
      case Some(packedArgs) => unpack(packedArgs)
    }
  }

  /**
    * Extracts the types from the given aggregate type.
    * A Unit unpacks to an empty list.
    * A Tuple unpacks to its member types.
    * Anything else unpacks to the singleton list of itself.
    */
  def unpack(tpe: Type): List[Type] = tpe.typeConstructor match {
    case Some(TypeConstructor.Unit) => Nil
    case Some(TypeConstructor.Tuple(_)) => tpe.typeArguments
    case _ => List(tpe)
  }

  /**
    * Creates a pattern corresponding to the given tag type.
    */
  def mkPattern(tpe: Type)(implicit flix: Flix): (ResolvedAst.Pattern, List[Symbol.VarSym]) = tpe.typeConstructor match {
    case Some(TypeConstructor.Tag(sym, tag)) =>
      getTagArguments(tpe) match {
        case Nil => (ResolvedAst.Pattern.Tag(sym, tag, ResolvedAst.Pattern.Unit(SourceLocation.Unknown), SourceLocation.Unknown), Nil)
        case _ :: Nil =>
          val varSym = Symbol.freshVarSym()
          (ResolvedAst.Pattern.Tag(sym, tag, ResolvedAst.Pattern.Var(varSym, SourceLocation.Unknown), SourceLocation.Unknown), List(varSym))
        case tpes =>
          val varSyms = tpes.map(_ => Symbol.freshVarSym())
          val subPats = varSyms.map(varSym => ResolvedAst.Pattern.Var(varSym, SourceLocation.Unknown))
          (ResolvedAst.Pattern.Tag(sym, tag, ResolvedAst.Pattern.Tuple(subPats, SourceLocation.Unknown), SourceLocation.Unknown), varSyms)
      }
    case _ => throw InternalCompilerException("Unexpected non-tag type.")
  }

  /**
    * Inserts `sep` between every two elements of `list`.
    */
  def intersperse[A](list: List[A], sep: A): List[A] = list match {
    case Nil => Nil
    case last :: Nil => last :: Nil
    case head :: neck :: tail => head :: sep :: intersperse(neck :: tail, sep)
  }
}
