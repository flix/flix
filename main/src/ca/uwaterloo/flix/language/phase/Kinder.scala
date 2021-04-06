package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Ast, Kind, ResolvedAst, Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation.ToSuccess

import scala.annotation.tailrec

object Kinder extends Phase[ResolvedAst.Root, ResolvedAst.Root] { // MATT change to KindedAst.Root ?


  /**
    * Runs the p
    */
  override def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Root, CompilationError] = {
    // visit enums
    // visit type aliases
    // visit classes
    // visit defs
    ???
  }

  private def visitEnum(enum: ResolvedAst.Enum, root: ResolvedAst.Root): Validation[ResolvedAst.Enum, CompilationError] = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, cases, _, _, _) =>
      val ascriptions = getAscriptions(tparams)
      val newCases = cases.map {
        case (_, caze@ResolvedAst.Case(enum, tag, tpeDeprecated, sc)) =>
          val newScheme = checkScheme(sc, ascriptions, root)
          (tag, caze.copy(sc = newScheme))
      }
      enum.copy(cases = newCases).toSuccess
  }

  // MATT need to add type aliases to ResolvedAst;
  // MATT what effect does the replacement in Resolver have?

  private def visitClass(clazz: ResolvedAst.Class, root: ResolvedAst.Root): Validation[ResolvedAst.Class, CompilationError] = clazz match {
    case ResolvedAst.Class(doc, mod, sym, tparam, superClasses, sigs, laws, loc) =>
      val ascriptions = getAscriptions(List(tparam))
      // MATT superclasses should be constraints instead
      // MATT check over class constraints with ascriptions
      // MATT check over sigs with ascriptions
      // MATT check over laws with ascriptions
      clazz.toSuccess
  }

  private def visitInstance(inst: ResolvedAst.Instance, root: ResolvedAst.Root): Validation[ResolvedAst.Instance, CompilationError] = inst match {
    case ResolvedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc) =>
      val clazz = root.classes(sym)
      val classAscriptions = getAscriptions(List(clazz.tparam))
      val kind = classAscriptions.head._2 // MATT make safer for multiparam classes if those ever come
      val expectedKind = KindMatch.fromKind(kind)

      val (actualKind, ascriptions) = inferKinds(tpe, expectedKind, root)
      // MATT need to return new type from this (?)
      // MATT use internal visit function to avoid extra "actualkind"

      val newTconstrs = tconstrs.map(checkKinds(_, ascriptions, root))

      // MATT check all the def kinds
      ???
  }

  // MATT only useful for instances b/c of complexity assumptions
  private def inferKinds(tpe: Type, expected: KindMatch, root: ResolvedAst.Root): (Kind, Map[Int, Kind]) = tpe match {
    case Type.Var(id, kind, rigidity, text) =>
      val k = KindMatch.toKind(expected)
      (k, Map(id -> k))
    case Type.Cst(tc, loc) =>
      val kind = tc match {
        case TypeConstructor.Enum(sym, _) => // ignore actual kind (?)
          getDeclaredKind(root.enums(sym))
        case _ => tc.kind
      }
      assert(KindMatch.matches(expected, kind)) // MATT monad
      (kind, Map.empty)
    case Type.Lambda(tvar, tpe) =>
      throw InternalCompilerException("TODO") // MATT

    case _: Type.Apply =>
      val (base, args) = baseAndArgs(tpe)
      val (baseKind, baseMap) = inferKinds(base, KindMatch.Wild, root) // wild is ok here because base is surely not a var

      val expectedArgs = argKinds(baseKind).map(KindMatch.fromKind)

      val (argKs, argMaps) = expectedArgs.zip(args).map {
        case (expected, argType) => inferKinds(argType, expected, root)
      }.unzip

      val kind = applyKind(baseKind, argKs)

      assert(KindMatch.matches(expected, kind)) // MATT monad

      val kindMap = argMaps.fold(baseMap)(_ ++ _)

      (kind, kindMap)

  }

  def applyKind(base: Kind, args: List[Kind]): Kind = {
    def apply1(base: Kind, arg: Kind): Kind = base match {
      case Kind.Arrow(k1, k2) if arg <:: k1 => k2
      case _ => throw InternalCompilerException("illegal kind application") // MATT actually do monad stuff
    }
    args.foldLeft(base)(apply1)
  }

  def baseAndArgs(tpe: Type): (Type, List[Type]) = {
    @tailrec
    def visit(tpe: Type, args: List[Type]): (Type, List[Type]) = tpe match {
      case Type.Apply(tpe1, tpe2) => visit(tpe1, tpe2 :: args)
      case _ => (tpe, args)
    }
    visit(tpe, Nil)
  }

  def argKinds(k: Kind): List[Kind] = {
    k match {
      case Kind.Arrow(k1, k2) => k1 :: argKinds(k2)
      case _ => Nil
    }
  }

  private def checkKinds(tconstr: Ast.TypeConstraint, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Ast.TypeConstraint = tconstr match {
    case Ast.TypeConstraint(sym, tpe, loc) =>
      val clazz = root.classes(sym)
      val classAscriptions = getAscriptions(List(clazz.tparam))
      val kind = classAscriptions.head._2 // MATT make safer for multiparam classes if those ever come
      val expectedKind = KindMatch.fromKind(kind)

      val newTpe = checkKinds(tpe, expectedKind, ascriptions, root)
      Ast.TypeConstraint(sym, newTpe, loc)

  }
  private def checkKinds(tpe: Type, expected: KindMatch, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Type = {

    def visit(tpe: Type, expected: KindMatch): Type = tpe match {
      case Type.Var(id, _, rigidity, text) => // ignore actual kind (?)
        val actual = ascriptions(id)
        assert(KindMatch.matches(expected, actual))
        Type.Var(id, actual, rigidity, text)
      case Type.Cst(tc, _) =>
        val kind = tc match {
          case TypeConstructor.Enum(sym, _) => // ignore actual kind (?)
            getDeclaredKind(root.enums(sym))
          case _ => tc.kind
        }
        assert(KindMatch.matches(expected, kind))
        tpe
      case Type.Apply(tpe1, tpe2) =>
        val newTpe2 = visit(tpe2, KindMatch.Wild)
        val newTpe1 = visit(tpe1, KindMatch.Arrow(KindMatch.fromKind(newTpe2.kind), expected))
        val newTpe = Type.Apply(newTpe1, newTpe2)
        assert(KindMatch.matches(expected, newTpe.kind))
        newTpe
      case Type.Lambda(tvar, tpe) =>
        val newTvar = visit(tvar, KindMatch.Wild)
        val newBody = visit(tpe, KindMatch.Wild)
        val newTpe = Type.Lambda(newTvar.asInstanceOf[Type.Var], newBody)
        assert(KindMatch.matches(expected, newTpe.kind))
        newTpe
    }

    visit(tpe, expected)
  }

  def getDeclaredKind(enum: ResolvedAst.Enum): Kind = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, _, _, _, _) =>
      tparams.foldRight(Kind.Star: Kind) { // MATT is foldRight right?
        case (tparam, acc) => tparam.tpe.kind ->: acc
      }
    // MATT use types to enforce explicit/implicit kinding invariant
  }

  def getAscriptions(tparams: List[ResolvedAst.TypeParam]): Map[Int, Kind] = {
    tparams.foldLeft(Map.empty[Int, Kind]) {
      case (acc, tparam) => acc + (tparam.tpe.id -> tparam.tpe.kind)
    }
  }

  def visitSpec(spec: ResolvedAst.Spec, root: ResolvedAst.Root): Validation[ResolvedAst.Spec, CompilationError] = spec match {
    case ResolvedAst.Spec(doc, ann, mod, tparams, fparams, sc, eff, loc) =>

      val explicit: Boolean = ???

      if (explicit) {
        val ascriptions = getAscriptions(tparams)

        val newScheme = checkScheme(sc, ascriptions, root)

        val newEff = checkKinds(eff, KindMatch.Bool, ascriptions, root)

        val newFparams = fparams.map {
          case ResolvedAst.FormalParam(sym, mod, tpe, loc) =>
            val newTpe = checkKinds(tpe, KindMatch.Star, ascriptions, root)
            ResolvedAst.FormalParam(sym, mod, newTpe, loc)
        }

        // MATT need to check exp?
        ResolvedAst.Spec(doc, ann, mod, tparams, newFparams, newScheme, newEff, loc).toSuccess
      } else {
        val fparamAscriptions = fparams.foldLeft(Map.empty[Int, Kind]) {
          case (acc, fparam) =>
            val (_kind, map)  = inferKinds(fparam.tpe, KindMatch.Star, root)
            acc ++ map // MATT handle conflicts
        }
        val (_effKind, effAscriptions) = inferKinds(eff, KindMatch.Bool, root)

        // MATT need to keep and use result of inference

        val ascriptions = fparamAscriptions ++ effAscriptions // MATT handle conflicts

        val newScheme = checkScheme(sc, ascriptions, root)

        ResolvedAst.Spec(doc, ann, mod, tparams, fparams, newScheme, eff, loc).toSuccess
      }
  }

  private def checkScheme(scheme: Scheme, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Scheme = scheme match {
    case Scheme(quantifiers, constraints, base) =>
      val newBase = checkKinds(base, KindMatch.Star, ascriptions, root)
      // MATT use better types to avoid cast
      val newQuantifiers = quantifiers.map(checkKinds(_, KindMatch.Wild, ascriptions, root).asInstanceOf[Type.Var])
      val newConstraints = constraints.map {
        // MATT check against known classes
        case Ast.TypeConstraint(sym, tpe, loc) => Ast.TypeConstraint(sym, checkKinds(tpe, KindMatch.Wild, ascriptions, root), loc)
      }
      Scheme(newQuantifiers, newConstraints, newBase)
  }

  private sealed trait KindMatch

  private object KindMatch {
    case object Wild extends KindMatch
    case class Arrow(k1: KindMatch, k2: KindMatch) extends KindMatch
    case object Bool extends KindMatch
    case object Star extends KindMatch
    case object Record extends KindMatch
    case object Schema extends KindMatch

    def fromKind(k: Kind): KindMatch = {
      k match {
        case Kind.Var(id) => ???
        case Kind.Star => Star
        case Kind.Bool => Bool
        case Kind.Record => Record
        case Kind.Schema => Schema
        case Kind.Arrow(k1, k2) => Arrow(fromKind(k1), fromKind(k2))
      }
    }

    def matches(k1: KindMatch, k2: Kind): Boolean = (k1, k2) match {
      case (Wild, _) => true
      case (Star, Kind.Star) => true
      case (Bool, Kind.Bool) => true
      case (Record, Kind.Record) => true
      case (Schema, Kind.Schema) => true
      case (Arrow(k11, k12),  Kind.Arrow(k21, k22)) => matches(k11, k21) && matches(k12, k22)
    }

    def toKind(k: KindMatch): Kind = k match {
      case Wild => throw InternalCompilerException("impossible maybe?") // MATT
      case Star => Kind.Star
      case Bool => Kind.Bool
      case Record => Kind.Record
      case Schema => Kind.Schema
      case Arrow(k1, k2) => toKind(k1) ->: toKind(k2)
    }
  }
}
