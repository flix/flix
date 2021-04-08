package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ResolvedAst.TypeParams
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, mapN, traverse}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.annotation.tailrec

object Kinder extends Phase[ResolvedAst.Root, ResolvedAst.Root] { // MATT change to KindedAst.Root ?
  // MATT license

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

  // MATT docs
  private def visitEnum(enum: ResolvedAst.Enum, root: ResolvedAst.Root): Validation[ResolvedAst.Enum, CompilationError] = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, cases, _, _, _) =>
      val ascriptions = getAscriptions(tparams)
      val newCasesVal = traverse(cases) {
        case (_, caze@ResolvedAst.Case(enum, tag, tpeDeprecated, sc)) =>
          checkScheme(sc, ascriptions, root) map {
            newScheme => (tag, caze.copy(sc = newScheme))
          }
      }
      mapN(newCasesVal) {
        newCases => enum.copy(cases = newCases.toMap)
      }
  }

  // MATT need to add type aliases to ResolvedAst;
  // MATT what effect does the replacement in Resolver have?

  private def visitClass(clazz: ResolvedAst.Class, root: ResolvedAst.Root): Validation[ResolvedAst.Class, CompilationError] = clazz match {
    case ResolvedAst.Class(doc, mod, sym, tparam, superClasses, sigs, laws, loc) =>
      val ascriptions = getAscription(tparam)

      // MATT superclasses should be constraints instead
      // MATT check over class constraints with ascriptions
      // MATT check over sigs with ascriptions
      // MATT check over laws with ascriptions
      clazz.toSuccess
  }

  // MATT docs
  private def visitInstance(inst: ResolvedAst.Instance, root: ResolvedAst.Root): Validation[ResolvedAst.Instance, CompilationError] = inst match {
    case ResolvedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc) =>
      val clazz = root.classes(sym)
      val classAscriptions = getAscription(clazz.tparam)
      val kind = classAscriptions._2 // MATT make safer for multiparam classes if those ever come
      val expectedKind = KindMatch.fromKind(kind)

      val (actualKind, ascriptions) = inferKinds(tpe, expectedKind, root)
      // MATT need to return new type from this (?)
      // MATT use internal visit function to avoid extra "actualkind"

      val newTconstrs = tconstrs.map(checkKinds(_, ascriptions, root))

      // MATT check all the def kinds
      ???
  }

  // MATT only useful for instances b/c of complexity assumptions
  private def inferKinds(tpe: Type, expected: KindMatch, root: ResolvedAst.Root): Validation[(Kind, Map[Int, Kind]), KindError] = tpe match {
    case Type.Var(id, kind, rigidity, text) =>
      val k = KindMatch.toKind(expected)
      (k, Map(id -> k)).toSuccess

    case Type.Cst(tc, loc) =>
      val kind = tc match {
        case TypeConstructor.Enum(sym, _) => // ignore actual kind (?)
          getDeclaredKind(root.enums(sym))
        case _ => tc.kind
      }
      checkKindsMatch(expected, kind) map {
        _ => (kind, Map.empty)
      }
    case Type.Lambda(tvar, tpe) =>
      throw InternalCompilerException("TODO") // MATT can't do without kind vars?

    case _: Type.Apply =>
      // MATT inline docs
      val (base, args) = baseAndArgs(tpe)
      for {
        res <- inferKinds(base, KindMatch.Wild, root) // wild is ok here because base is surely not a var
        (baseKind, baseMap) = res
        expectedArgs = argKinds(baseKind).map(KindMatch.fromKind)
        res <- traverse(args.zip(expectedArgs)) { case (arg, expectedArg) => inferKinds(arg, expectedArg, root) }
        (argKs, argMaps) = res.unzip
        kind = applyKind(baseKind, argKs)
        _ <- checkKindsMatch(expected, kind)
        kindMap = argMaps.fold(baseMap)(_ ++ _) // MATT need to check for conflicts?
      } yield (kind, kindMap)
  }

  // MATT docs
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

  private def checkKinds(tconstr: Ast.TypeConstraint, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case Ast.TypeConstraint(sym, tpe, loc) =>
      val clazz = root.classes(sym)
      val classAscriptions = getAscription(clazz.tparam)
      val kind = classAscriptions._2 // MATT make safer for multiparam classes if those ever come
      val expectedKind = KindMatch.fromKind(kind)

      checkKinds(tpe, expectedKind, ascriptions, root) map {
        newTpe => Ast.TypeConstraint(sym, newTpe, loc)
      }

  }

  private def checkKinds(tpe: Type, expected: KindMatch, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Type, KindError] = {

    def visit(tpe: Type, expected: KindMatch): Validation[Type, KindError] = tpe match {
      case Type.Var(id, _, rigidity, text) => // ignore actual kind (?)
        val actual = ascriptions(id)
        checkKindsMatch(expected, actual) map {
          _ => Type.Var(id, actual, rigidity, text)
        }
      case Type.Cst(tc, _) =>
        val kind = tc match {
          case TypeConstructor.Enum(sym, _) => // ignore actual kind (?)
            getDeclaredKind(root.enums(sym))
          case _ => tc.kind
        }
        checkKindsMatch(expected, kind) map {
          _ => tpe
        }
      case Type.Apply(tpe1, tpe2) =>
        for {
          newTpe2 <- visit(tpe2, KindMatch.Wild)
          newTpe1 <- visit(tpe1, KindMatch.Arrow(KindMatch.fromKind(newTpe2.kind), expected))
          newTpe = Type.Apply(newTpe1, newTpe2)
          _ <- checkKindsMatch(expected, newTpe.kind)
        } yield newTpe

      case Type.Lambda(tvar, tpe) =>
        for {
          newTvar <- visit(tvar, KindMatch.Wild)
          newBody <- visit(tpe, KindMatch.Wild)
          newTpe = Type.Lambda(newTvar.asInstanceOf[Type.Var], newBody) // MATT avoid cast if possible
          _ <- checkKindsMatch(expected, newTpe.kind)
        } yield newTpe
    }

    visit(tpe, expected)
  }

  // MATT docs
  def getDeclaredKind(enum: ResolvedAst.Enum): Kind = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, _, _, _, _) =>
      val ascriptions = getAscriptions(tparams)
      tparams.tparams.foldRight(Kind.Star: Kind) { // MATT is foldRight right?
        case (tparam, acc) => ascriptions(tparam.tpe.id) ->: acc
      }
    // MATT use types to enforce explicit/implicit kinding invariant
  }

  // MATT docs
  def getAscriptions(tparams0: ResolvedAst.TypeParams): Map[Int, Kind] = tparams0 match {
      // Case 1: Kinded tparams: use their kinds
    case ResolvedAst.TypeParams.Kinded(tparams) => tparams.foldLeft(Map.empty[Int, Kind]) {
      case (acc, ResolvedAst.TypeParam.Kinded(_, tpe, kind, _)) => acc + (tpe.id -> kind)
    }
      // Case 2: Unkinded tparams: default to Star kind
    case ResolvedAst.TypeParams.Unkinded(tparams) =>
      tparams.foldLeft(Map.empty[Int, Kind]) {
        case (acc, tparam) => acc + (tparam.tpe.id -> Kind.Star)
      }
  }

  // MATT docs
  def getAscription(tparam0: ResolvedAst.TypeParam): (Int, Kind) = tparam0 match {
      // MATT case docs
    case ResolvedAst.TypeParam.Kinded(_, tpe, kind, _) => tpe.id -> kind
    case ResolvedAst.TypeParam.Unkinded(_, tpe, _) => tpe.id -> Kind.Star
  }

  def visitSpec(spec: ResolvedAst.Spec, root: ResolvedAst.Root): Validation[ResolvedAst.Spec, CompilationError] = spec match {
    case ResolvedAst.Spec(doc, ann, mod, tparams0, fparams, sc, eff, loc) => tparams0 match {
      case TypeParams.Kinded(tparams) =>
        val ascriptions = getAscriptions(tparams0)

        val newScheme = checkScheme(sc, ascriptions, root)

        val newEff = checkKinds(eff, KindMatch.Bool, ascriptions, root)


        val newFparams = fparams.map {
          case ResolvedAst.FormalParam(sym, mod, tpe, loc) =>
            val newTpe = checkKinds(tpe, KindMatch.Star, ascriptions, root)
            ResolvedAst.FormalParam(sym, mod, newTpe, loc)
        }

        // MATT need to check exp?
        ResolvedAst.Spec(doc, ann, mod, tparams, newFparams, newScheme, newEff, loc).toSuccess
      case TypeParams.Unkinded(tparams) =>
        val fparamAscriptions = fparams.foldLeft(Map.empty[Int, Kind]) {
          case (acc, fparam) =>
            val (_kind, map) = inferKinds(fparam.tpe, KindMatch.Star, root)
            acc ++ map // MATT handle conflicts
        }
        val (_effKind, effAscriptions) = inferKinds(eff, KindMatch.Bool, root)

        // MATT need to keep and use result of inference

        val ascriptions = fparamAscriptions ++ effAscriptions // MATT handle conflicts

        val newScheme = checkScheme(sc, ascriptions, root)

        ResolvedAst.Spec(doc, ann, mod, tparams, fparams, newScheme, eff, loc).toSuccess
    }
  }

  private def checkScheme(scheme: Scheme, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Scheme, KindError] = scheme match {
    case Scheme(quantifiers, constraints, base) =>
      val newBaseVal = checkKinds(base, KindMatch.Star, ascriptions, root)
      // MATT use better types to avoid cast
      val newQuantifiersVal = traverse(quantifiers)(checkKinds(_, KindMatch.Wild, ascriptions, root).map(_.asInstanceOf[Type.Var])) // MATT avoid cast ?
      val newConstraintsVal = traverse(constraints)(checkKinds(_, ascriptions, root))
      mapN(newQuantifiersVal, newConstraintsVal, newBaseVal) {
        case (newQuantifiers, newConstraints, newBase) => Scheme(newQuantifiers, newConstraints, newBase)
      }
  }

  private def checkKindsMatch(k1: KindMatch, k2: Kind): Validation[Unit, KindError] = {
    if (KindMatch.matches(k1, k2)) {
      ().toSuccess
    } else {
      KindError.MismatchedKinds(KindMatch.toKind(k1), k2, SourceLocation.Unknown).toFailure // MATT real location
      // MATT don't do toKind
    }

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
