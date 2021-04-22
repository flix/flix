package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ResolvedAst.{TypeParam, TypeParams}
import ca.uwaterloo.flix.language.ast.UnkindedType.Constructor
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, fold, mapN, traverse}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.annotation.tailrec

object Kinder extends Phase[ResolvedAst.Root, KindedAst.Root] { // MATT change to KindedAst.Root ?
  // MATT license

  /**
    * Runs the p
    */
  override def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, CompilationError] = {
    // visit enums
    // visit type aliases
    // visit classes
    // visit defs
    ???
  }

  // MATT docs
  private def visitEnum(enum: ResolvedAst.Enum, root: ResolvedAst.Root): Validation[KindedAst.Enum, CompilationError] = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams0, cases, tpeDeprecated, sc, loc) =>
      val ascriptions = getAscriptions(tparams0)
      val newCasesVal = traverse(cases) {
        case (_, ResolvedAst.Case(enum, tag, tpeDeprecated, sc)) =>
          val schemeVal = checkScheme(sc, ascriptions, root)
          val tpeVal = checkKinds(tpeDeprecated, KindMatch.Star, ascriptions, root)
          mapN(schemeVal, tpeVal) {
            case (scheme, tpe) => (tag, KindedAst.Case(enum, tag, tpe, scheme))
          }
      }

      val newSchemeVal = checkScheme(sc, ascriptions, root)
      val newTparamsVal = visitTparams(tparams0, ascriptions, root) // MATT merge with getAscriptions
      val tpeVal = checkKinds(tpeDeprecated, KindMatch.Star, ascriptions, root)
      mapN(newCasesVal, newSchemeVal, newTparamsVal, tpeVal) {
        case (newCases, newScheme, tparams, tpe) =>
          KindedAst.Enum(doc, mod, sym, tparams, newCases.toMap, tpe, newScheme, loc)
      }
  }

  // MATT need to add type aliases to ResolvedAst;
  // MATT what effect does the replacement in Resolver have?

  private def visitClass(clazz: ResolvedAst.Class, root: ResolvedAst.Root): Validation[KindedAst.Class, CompilationError] = clazz match {
    case ResolvedAst.Class(doc, mod, sym, tparam0, superClasses0, sigs0, laws0, loc) =>
      val ascriptions = Map(getAscription(tparam0))
      val superClassesVal = traverse(superClasses0)(checkKinds(_, ascriptions, root))
      val sigsVal = traverse(sigs0) {
        case (sigSym, sig0) => visitSig(sig0, ascriptions, root).map(sig => sigSym -> sig)
      }
      val lawsVal = traverse(laws0)(visitDef(_, ascriptions, root))
      val tparamVal = visitTparam(tparam0, ascriptions, root)

      mapN(superClassesVal, sigsVal, lawsVal, tparamVal) {
        case (superClasses, sigs, laws, tparam) => KindedAst.Class(doc, mod, sym, tparam, superClasses, sigs.toMap, laws, loc)
      }
  }

  // MATT docs
  private def visitInstance(inst: ResolvedAst.Instance, root: ResolvedAst.Root): Validation[KindedAst.Instance, CompilationError] = inst match {
    case ResolvedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc) =>
      val clazz = root.classes(sym)
      val classAscriptions = Map(getAscription(clazz.tparam))
      val kind = classAscriptions.head._2 // MATT make safer for multiparam classes if those ever come
      val expectedKind = KindMatch.fromKind(kind)


      inferKinds(tpe, expectedKind, root) flatMap {
        case (actualKind, ascriptions) =>
          // MATT need to return new type from this (?)
          // MATT use internal visit function to avoid extra "actualkind"
          val newTconstrsVal = traverse(tconstrs)(checkKinds(_, ascriptions, root))
          val newDefsVal = traverse(defs)(visitDef(_, ascriptions, root))
          val tpeVal = checkKinds(tpe, KindMatch.fromKind(actualKind), ascriptions, root) // MATT this is redundant with inferKinds
          mapN(newTconstrsVal, newDefsVal, tpeVal) {
            case (newTconstrs, newDefs, newTpe) => KindedAst.Instance(doc, mod, sym, newTpe, newTconstrs, newDefs, ns, loc)
          }
      }
  }

  // MATT docs
  private def visitAnnotation(ann: ResolvedAst.Annotation, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Annotation, KindError] = ann match {
    case ResolvedAst.Annotation(name, exps, loc) =>
      KindedAst.Annotation(name, ???, loc).toSuccess // MATT
  }

  // MATT docs
  private def visitTparams(tparams0: ResolvedAst.TypeParams, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[List[KindedAst.TypeParam], KindError] = tparams0 match {
    case ResolvedAst.TypeParams.Kinded(tparams) => tparams.map {
      tparam => KindedAst.TypeParam(???, ???, ???, ???)
    }.toSuccess // MATT
  }

  // MATT docs
  private def visitTparam(tparam0: ResolvedAst.TypeParam, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.TypeParam, KindError] = tparam0 match {
    case TypeParam.Kinded(name, tpe, kind, loc) => ??? // MATT
    case TypeParam.Unkinded(name, tpe, loc) => ??? // MATT
  }

  // MATT docs
  private def visitSig(sig0: ResolvedAst.Sig, ascriptions0: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, exp0) =>
      for {
        res <- visitSpec(spec0, ascriptions0, root)
        (spec, ascriptions) = res
        exp <- traverse(exp0)(visitExp(_, ascriptions, root))
      } yield KindedAst.Sig(sym, spec, exp.headOption)
  }

  // MATT docs
  private def visitDef(defn0: ResolvedAst.Def, ascriptions0: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Def, KindError] = defn0 match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      for {
        res <- visitSpec(spec0, ascriptions0, root)
        (spec, ascriptions) = res
        exp <- visitExp(exp0, ascriptions, root)
      } yield KindedAst.Def(sym, spec, exp)
  }

  // MATT docs
  private def visitExp(exp0: ResolvedAst.Expression, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Expression, KindError] = {
    ??? // MATT
  }

  // MATT only useful for instances b/c of complexity assumptions
  private def inferKinds(tpe: UnkindedType, expected: KindMatch, root: ResolvedAst.Root): Validation[(Kind, Map[Int, Kind]), KindError] = tpe match {
    case UnkindedType.Var(id, text) =>
      val k = KindMatch.toKind(expected)
      (k, Map(id -> k)).toSuccess

    case UnkindedType.Cst(tc0, loc) =>
      val tc = visitTypeConstructor(tc0, root)
      checkKindsMatch(expected, tc.kind) map {
        _ => (tc.kind, Map.empty)
      }
    case UnkindedType.Lambda(tvar, tpe) =>
      throw InternalCompilerException("TODO") // MATT can't do without kind vars?

    case _: UnkindedType.Apply =>
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

  def baseAndArgs(tpe: UnkindedType): (UnkindedType, List[UnkindedType]) = {
    @tailrec
    def visit(tpe: UnkindedType, args: List[UnkindedType]): (UnkindedType, List[UnkindedType]) = tpe match {
      case UnkindedType.Apply(tpe1, tpe2) => visit(tpe1, tpe2 :: args)
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

  private def checkKinds(tconstr: ResolvedAst.TypeConstraint, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(sym, tpe, loc) =>
      val clazz = root.classes(sym)
      val classAscriptions = getAscription(clazz.tparam)
      val kind = classAscriptions._2 // MATT make safer for multiparam classes if those ever come
      val expectedKind = KindMatch.fromKind(kind)

      checkKinds(tpe, expectedKind, ascriptions, root) map {
        newTpe => Ast.TypeConstraint(sym, newTpe, loc)
      }

  }

  private def checkKinds(tpe: UnkindedType, expected: KindMatch, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Type, KindError] = {

    def visit(tpe: UnkindedType, expected: KindMatch): Validation[Type, KindError] = tpe match {
      case UnkindedType.Var(id, text) =>
        val actual = ascriptions(id)
        checkKindsMatch(expected, actual) map {
          _ => Type.Var(id, actual, text = text)
        }
      case UnkindedType.Cst(tc0, loc) =>
        val tc = visitTypeConstructor(tc0, root)
        checkKindsMatch(expected, tc.kind) map {
          _ => Type.Cst(tc, loc)
        }
      case UnkindedType.Apply(tpe1, tpe2) =>
        for {
          newTpe2 <- visit(tpe2, KindMatch.Wild)
          newTpe1 <- visit(tpe1, KindMatch.Arrow(KindMatch.fromKind(newTpe2.kind), expected))
          newTpe = Type.Apply(newTpe1, newTpe2)
          _ <- checkKindsMatch(expected, newTpe.kind)
        } yield newTpe

      case UnkindedType.Lambda(tvar, tpe) =>
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
  def visitTypeConstructor(tycon: UnkindedType.Constructor, root: ResolvedAst.Root): TypeConstructor = tycon match {
    case UnkindedType.Constructor.Unit => TypeConstructor.Unit
    case UnkindedType.Constructor.Null => TypeConstructor.Null
    case UnkindedType.Constructor.Bool => TypeConstructor.Bool
    case UnkindedType.Constructor.Char => TypeConstructor.Char
    case UnkindedType.Constructor.Float32 => TypeConstructor.Float32
    case UnkindedType.Constructor.Float64 => TypeConstructor.Float64
    case UnkindedType.Constructor.Int8 => TypeConstructor.Int8
    case UnkindedType.Constructor.Int16 => TypeConstructor.Int16
    case UnkindedType.Constructor.Int32 => TypeConstructor.Int32
    case UnkindedType.Constructor.Int64 => TypeConstructor.Int64
    case UnkindedType.Constructor.BigInt => TypeConstructor.BigInt
    case UnkindedType.Constructor.Str => TypeConstructor.Str
    case UnkindedType.Constructor.Arrow(arity) => TypeConstructor.Arrow(arity)
    case UnkindedType.Constructor.RecordEmpty => TypeConstructor.RecordEmpty
    case UnkindedType.Constructor.RecordExtend(field) => TypeConstructor.RecordExtend(field)
    case UnkindedType.Constructor.SchemaEmpty => TypeConstructor.SchemaEmpty
    case UnkindedType.Constructor.SchemaExtend(pred) => TypeConstructor.SchemaExtend(pred)
    case UnkindedType.Constructor.Array => TypeConstructor.Array
    case UnkindedType.Constructor.Channel => TypeConstructor.Channel
    case UnkindedType.Constructor.Lazy => TypeConstructor.Lazy
    case UnkindedType.Constructor.Tag(sym, tag) => TypeConstructor.Tag(sym, tag)
    case UnkindedType.Constructor.Enum(sym) =>
      // Lookup the enum kind
      val kind = getDeclaredKind(root.enums(sym)) // MATT any reason to expect a bad lookup here?
      TypeConstructor.Enum(sym, kind)
    case UnkindedType.Constructor.Native(clazz) => TypeConstructor.Native(clazz)
    case UnkindedType.Constructor.Ref => TypeConstructor.Ref
    case UnkindedType.Constructor.Tuple(l) => TypeConstructor.Tuple(l)
    case UnkindedType.Constructor.Relation => TypeConstructor.Relation
    case UnkindedType.Constructor.Lattice => TypeConstructor.Lattice
    case UnkindedType.Constructor.True => TypeConstructor.True
    case UnkindedType.Constructor.False => TypeConstructor.False
    case UnkindedType.Constructor.Not => TypeConstructor.Not
    case UnkindedType.Constructor.And => TypeConstructor.And
    case UnkindedType.Constructor.Or => TypeConstructor.Or
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

  def visitSpec(spec: ResolvedAst.Spec, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[(KindedAst.Spec, Map[Int, Kind]), KindError] = spec match {
      // MATT need to compare/merge with provided ascriptions
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams, sc, eff, loc) => tparams0 match {
      case TypeParams.Kinded(tparams) =>
        val ascriptions = getAscriptions(tparams0)

        val newSchemeVal = checkScheme(sc, ascriptions, root)

        val newEffVal = checkKinds(eff, KindMatch.Bool, ascriptions, root)


        val newFparamsVal = traverse(fparams) {
          case ResolvedAst.FormalParam(sym, mod, tpe, loc) =>
            checkKinds(tpe, KindMatch.Star, ascriptions, root) map {
              newTpe => KindedAst.FormalParam(sym, mod, newTpe, loc)
            }
        }

        val newAnnVal = traverse(ann0)(visitAnnotation(_, ascriptions, root))

        val newTparamsVal = visitTparams(tparams0, ascriptions, root)

        mapN(newSchemeVal, newEffVal, newFparamsVal, newAnnVal, newTparamsVal) {
          case (newScheme, newEff, newFparams, newAnn, newTparams) =>
            // MATT do arg0 -> arg instead of arg -> newArg
            (KindedAst.Spec(doc, newAnn, mod, newTparams, newFparams, newScheme, newEff, loc), ascriptions)
        }
      case TypeParams.Unkinded(tparams) =>
        val fparamAscriptionsVal = fold(fparams, Map.empty[Int, Kind]) {
          case (acc, fparam) => inferKinds(fparam.tpe, KindMatch.Star, root) map {
            case (_kind, map) => acc ++ map // MATT handle conflicts
          }
        }

        // MATT visit fparams while inferring
        val newFparamsVal = traverse(fparams) {
          case ResolvedAst.FormalParam(sym, mod, tpe, loc) =>
            checkKinds(tpe, KindMatch.Star, ascriptions, root) map {
              newTpe => KindedAst.FormalParam(sym, mod, newTpe, loc)
            }
        }
        val effAscriptionsVal = inferKinds(eff, KindMatch.Bool, root) map {
          case (_, ascriptions) => ascriptions
        }

        val newAnnVal = traverse(ann0)(visitAnnotation(_, ascriptions, root))

        val newTparamsVal = visitTparams(tparams0, ascriptions, root)

        // MATT need to keep and use result of inference

        // MATT more docs everywhere
        flatMapN(fparamAscriptionsVal, effAscriptionsVal, newAnnVal, newTparamsVal, newFparamsVal) {
          case (fparamAscriptions, effAscriptions, newAnn, newTparams, newFparams) =>
            val ascriptions = fparamAscriptions ++ effAscriptions // MATT handle conflicts
            // MATT make merge ascriptions function
            // MATT visit eff
            checkScheme(sc, ascriptions, root) map {
              newScheme => (KindedAst.Spec(doc, newAnn, mod, newTparams, newFparams, newScheme, ???, loc), ascriptions)
            }
        }
    }
  }

  // MATT docs
  private def checkScheme(scheme: ResolvedAst.Scheme, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Scheme, KindError] = scheme match {
    case ResolvedAst.Scheme(quantifiers, constraints, base) =>
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
      // MATT don't do toKind (?)
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
