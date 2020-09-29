/*
 *  Copyright 2017 Magnus Madsen
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

import java.lang.reflect.{Constructor, Field, Method, Modifier}

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Denotation
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.ResolvedAst.Sig
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

/**
  * The Resolver phase performs name resolution on the program.
  */
object Resolver extends Phase[NamedAst.Root, ResolvedAst.Root] {

  private type SigLookup = Map[Name.NName, Map[String, NamedAst.Sig]]

  /**
    * The maximum depth to which type aliases are unfolded.
    */
  val RecursionLimit: Int = 25

  /**
    * Performs name resolution on the given program `root`.
    */
  def run(root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Root, ResolutionError] = flix.phase("Resolver") {

    // build a map for looking up signatures directly rather than searching through classes
    val sigs = buildSigLookup(root.classes)

    val classesVal = root.classes.flatMap {
      case (ns0, classes) => classes.map {
        case (_, clazz) => resolve(clazz, ns0, root, sigs) map {
          case s => s.sym -> s
        }
      }
    }

    val definitionsVal = root.defs.flatMap {
      case (ns0, defs) => defs.map {
        case (_, defn) => resolve(defn, ns0, root, sigs) map {
          case d => d.sym -> d
        }
      }
    }

    val enumsVal = root.enums.flatMap {
      case (ns0, enums) => enums.map {
        case (_, enum) => resolve(enum, ns0, root) map {
          case d => d.sym -> d
        }
      }
    }

    val latticeComponentsVal = root.latticesOps.map {
      case (tpe0, lattice0) =>
        for {
          tpe <- lookupType(tpe0, lattice0.ns, root)
          lattice <- resolve(lattice0, lattice0.ns, root, sigs)
        } yield (tpe, lattice)
    }

    val propertiesVal = traverse(root.properties) {
      case (ns0, properties) => Properties.resolve(properties, ns0, root, sigs)
    }

    for {
      classes <- sequence(classesVal).map(_ ++ mkSynthClasses())
      definitions <- sequence(definitionsVal)
      enums <- sequence(enumsVal)
      latticeComponents <- sequence(latticeComponentsVal)
      properties <- propertiesVal
    } yield ResolvedAst.Root(
      classes.toMap, definitions.toMap, enums.toMap, latticeComponents.toMap, properties.flatten, root.reachable, root.sources
    )
  }

  object Constraints {

    /**
      * Performs name resolution on the given `constraints` in the given namespace `ns0`.
      */
    def resolve(constraints: List[NamedAst.Constraint], tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[List[ResolvedAst.Constraint], ResolutionError] = {
      traverse(constraints)(c => resolve(c, tenv0, ns0, root, sigs))
    }

    /**
      * Performs name resolution on the given constraint `c0` in the given namespace `ns0`.
      */
    def resolve(c0: NamedAst.Constraint, tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Constraint, ResolutionError] = {
      for {
        ps <- traverse(c0.cparams)(p => Params.resolve(p, ns0, root))
        h <- Predicates.Head.resolve(c0.head, tenv0, ns0, root, sigs)
        bs <- traverse(c0.body)(b => Predicates.Body.resolve(b, tenv0, ns0, root, sigs))
      } yield ResolvedAst.Constraint(ps, h, bs, c0.loc)
    }

  }

  /**
    * Performs name resolution on the given typeclass `c0` in the given namespace `ns0`.
    */
  def resolve(c0: NamedAst.Class, ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Class, ResolutionError] = c0 match {
    case NamedAst.Class(doc, mod, sym, tparam0, signatures, loc) =>
      for {
        tparams <- resolveTypeParams(List(tparam0), ns0, root)
        sigs <- traverse(signatures)(resolve(_, ns0, root, sigs))
      } yield ResolvedAst.Class(doc, mod, sym, tparams.head, sigs, loc)
  }

  /**
    * Performs name resolution on the given signature `s0` in the given namespace `ns0`.
    */
  def resolve(s0: NamedAst.Sig, ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Sig, ResolutionError] = s0 match {
    case NamedAst.Sig(doc, ann0, mod, sym, tparams0, fparams0, sc0, eff0, loc) =>
      for {
        fparams <- resolveFormalParams(fparams0, ns0, root)
        tparams <- resolveTypeParams(tparams0, ns0, root)
        ann <- traverse(ann0)(visitAnnotation(_, ns0, root, sigs))
        scheme <- resolveScheme(sc0, ns0, root)
        eff <- lookupType(eff0, ns0, root)
      } yield ResolvedAst.Sig(doc, ann, mod, sym, tparams, fparams, scheme, eff, loc)
  }

  /**
    * Performs name resolution on the given definition `d0` in the given namespace `ns0`.
    */
  def resolve(d0: NamedAst.Def, ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Def, ResolutionError] = d0 match {
    case NamedAst.Def(doc, ann, mod, sym, tparams0, fparams0, exp0, sc0, eff0, loc) =>
      val fparam = fparams0.head

      for {
        fparamType <- lookupType(fparam.tpe, ns0, root)
        fparams <- resolveFormalParams(fparams0, ns0, root)
        tparams <- resolveTypeParams(tparams0, ns0, root)
        ann <- traverse(ann)(visitAnnotation(_, ns0, root, sigs))
        exp <- Expressions.resolve(exp0, Map(fparam.sym -> fparamType), ns0, root, sigs)
        scheme <- resolveScheme(sc0, ns0, root)
        eff <- lookupType(eff0, ns0, root)
      } yield ResolvedAst.Def(doc, ann, mod, sym, tparams, fparams, exp, scheme, eff, loc)
  }

  /**
    * Performs name resolution on the given enum `e0` in the given namespace `ns0`.
    */
  def resolve(e0: NamedAst.Enum, ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Enum, ResolutionError] = {
    val tparamsVal = traverse(e0.tparams)(p => Params.resolve(p, ns0, root))
    val casesVal = traverse(e0.cases) {
      case (name, NamedAst.Case(enum, tag, tpe)) =>
        for {
          tparams <- tparamsVal
          t <- lookupType(tpe, ns0, root)
          _ <- checkProperType(t, tag.loc)
        } yield {
          val freeVars = e0.tparams.map(_.tpe)
          val caseType = t
          val enumType = Type.mkEnum(e0.sym, freeVars)
          val base = Type.mkTag(e0.sym, tag.name, caseType, enumType)
          val sc = Scheme(freeVars, base)
          name -> ResolvedAst.Case(enum, tag, t, sc)
        }
    }
    for {
      tparams <- tparamsVal
      cases <- casesVal
      tpe <- lookupType(e0.tpe, ns0, root)
    } yield {
      val sc = Scheme(tparams.map(_.tpe), tpe)
      ResolvedAst.Enum(e0.doc, e0.mod, e0.sym, tparams, cases.toMap, tpe, sc, e0.loc)
    }
  }

  /**
    * Performs name resolution on the given lattice `l0` in the given namespace `ns0`.
    */
  def resolve(l0: NamedAst.LatticeOps, ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.LatticeOps, ResolutionError] = {
    val tenv0 = Map.empty[Symbol.VarSym, Type]
    for {
      tpe <- lookupType(l0.tpe, ns0, root)
      bot <- Expressions.resolve(l0.bot, tenv0, ns0, root, sigs)
      top <- Expressions.resolve(l0.top, tenv0, ns0, root, sigs)
      equ <- Expressions.resolve(l0.equ, tenv0, ns0, root, sigs)
      leq <- Expressions.resolve(l0.leq, tenv0, ns0, root, sigs)
      lub <- Expressions.resolve(l0.lub, tenv0, ns0, root, sigs)
      glb <- Expressions.resolve(l0.glb, tenv0, ns0, root, sigs)
    } yield ResolvedAst.LatticeOps(tpe, bot, top, equ, leq, lub, glb, ns0, l0.loc)
  }

  /**
    * Performs name resolution on the given attribute `a0` in the given namespace `ns0`.
    */
  private def visitAttribute(a0: NamedAst.Attribute, ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Attribute, ResolutionError] = {
    for {
      tpe <- lookupType(a0.tpe, ns0, root)
    } yield ResolvedAst.Attribute(a0.ident, tpe, a0.loc)
  }

  /**
    * Performs name resolution on the given annotation `a0` in the given namespace `ns0`.
    */
  private def visitAnnotation(a0: NamedAst.Annotation, ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Annotation, ResolutionError] = {
    for {
      args <- traverse(a0.args)(Expressions.resolve(_, Map.empty, ns0, root, sigs))
    } yield ResolvedAst.Annotation(a0.name, args, a0.loc)
  }

  object Expressions {

    /**
      * Performs name resolution on the given expression `exp0` in the namespace `ns0`.
      */
    // TODO: Why is this tenv here?
    def resolve(exp0: NamedAst.Expression, tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Expression, ResolutionError] = {

      /**
        * Creates `arity` fresh fparams for use in a curried def or sig application.
        */
      def mkFreshFparams(arity: Int, loc: SourceLocation): List[ResolvedAst.FormalParam] = {
        // Introduce a fresh variable symbol for each argument of the function definition.
        val varSyms = (0 until arity).map(i => Symbol.freshVarSym("$" + i)).toList

        // Introduce a formal parameter for each variable symbol.
        varSyms.map(sym => ResolvedAst.FormalParam(sym, Ast.Modifiers.Empty, sym.tvar, loc))
      }

      /**
        * Creates a lambda for use in a curried dif or sig application.
        */
      def mkCurriedLambda(fparams: List[ResolvedAst.FormalParam], baseExp: ResolvedAst.Expression, loc: SourceLocation): ResolvedAst.Expression = {
        // The arguments passed to the definition (i.e. the fresh variable symbols).
        val argExps = fparams.map(fparam => ResolvedAst.Expression.Var(fparam.sym, fparam.sym.tvar, loc))

        // The apply expression inside the lambda.
        val applyExp = ResolvedAst.Expression.Apply(baseExp, argExps, Type.freshVar(Kind.Star), Type.freshVar(Kind.Bool), loc)

        // The curried lambda expressions.
        fparams.foldRight(applyExp: ResolvedAst.Expression) {
          case (fparam, acc) => ResolvedAst.Expression.Lambda(fparam, acc, Type.freshVar(Kind.Star), loc)
        }
      }

      def visitDef(defn: NamedAst.Def, tvar: Type.Var, loc: SourceLocation): ResolvedAst.Expression = {
        //
        // We must curry the definition. Otherwise we would not be here.
        //
        // We introduce /n/ lambda expressions around the definition expression.
        //

        // Find the arity of the function definition.
        val arity = defn.fparams.length

        // Create the fresh fparams
        val fparams = mkFreshFparams(arity, loc)

        // The definition expression.
        val defExp = ResolvedAst.Expression.Def(defn.sym, tvar, loc)

        // Create and apply the lambda expressions
        mkCurriedLambda(fparams, defExp, loc)
      }

      def visitSig(sig: NamedAst.Sig, tvar: Type.Var, loc: SourceLocation): ResolvedAst.Expression = {
        //
        // We must curry the definition. Otherwise we would not be here.
        //
        // We introduce /n/ lambda expressions around the definition expression.
        //

        // Find the arity of the function definition.
        val arity = sig.fparams.length

        // Create the fresh fparams
        val fparams = mkFreshFparams(arity, loc)

        // The definition expression.
        val defExp = ResolvedAst.Expression.Sig(sig.sym, tvar, loc)

        // Create and apply the lambda expressions
        mkCurriedLambda(fparams, defExp, loc)
      }

      def visitApply(exp: NamedAst.Expression, exps: List[NamedAst.Expression], loc: SourceLocation): Validation[ResolvedAst.Expression, ResolutionError] = {
        for {
          e <- visit(exp, tenv0)
          es <- traverse(exps)(visit(_, tenv0))
        } yield {
          es.foldLeft(e) {
            case (acc, a) => ResolvedAst.Expression.Apply(acc, List(a), Type.freshVar(Kind.Star), Type.freshVar(Kind.Bool), loc)
          }
        }
      }


      /**
        * Local visitor.
        */
      def visit(e0: NamedAst.Expression, tenv0: Map[Symbol.VarSym, Type]): Validation[ResolvedAst.Expression, ResolutionError] = e0 match {

        case NamedAst.Expression.Wild(tvar, loc) =>
          ResolvedAst.Expression.Wild(tvar, loc).toSuccess

        case NamedAst.Expression.Var(sym, loc) => tenv0.get(sym) match {
          case None => ResolvedAst.Expression.Var(sym, sym.tvar, loc).toSuccess
          case Some(tpe) => ResolvedAst.Expression.Var(sym, tpe, loc).toSuccess
        }

        case NamedAst.Expression.DefSig(qname, tvar, loc) =>
          (lookupDef(qname, ns0, root), lookupSig(qname, ns0, sigs)) match {
            // Case 1: both def and sig found
            case (Validation.Success(defn), Validation.Success(sig)) =>
              ResolutionError.AmbiguousName(qname, ns0, List(defn.loc, sig.loc), loc).toFailure
            // Case 2: only def found
            case (Validation.Success(defn), Validation.Failure(_)) =>
              visitDef(defn, tvar, loc).toSuccess
            // Case 3: only sig found
            case (Validation.Failure(_), Validation.Success(sig)) =>
              visitSig(sig, tvar, loc).toSuccess
            // Case 4: neither def nor sig found
            case (Validation.Failure(defErrs), sigFail@Validation.Failure(sigErrs)) =>
               Validation.Failure(defErrs ++ sigErrs)// MATT handle the different errors better; inaccessible beats notfound
          }

        case NamedAst.Expression.Hole(nameOpt, tpe, evar, loc) =>
          val sym = nameOpt match {
            case None => Symbol.freshHoleSym(loc)
            case Some(name) => Symbol.mkHoleSym(ns0, name)
          }
          ResolvedAst.Expression.Hole(sym, tpe, evar, loc).toSuccess

        case NamedAst.Expression.Use(use, exp, loc) =>
          // Lookup the used name to ensure that it exists.
          use match {
            case NamedAst.Use.UseDef(qname, _, _) =>
              flatMapN(lookupDef(qname, ns0, root))(_ => visit(exp, tenv0))

            case NamedAst.Use.UseTyp(qname, _, _) =>
              flatMapN(lookupType(NamedAst.Type.Ambiguous(qname, loc), ns0, root))(_ => visit(exp, tenv0))

            case NamedAst.Use.UseTag(qname, tag, _, _) =>
              flatMapN(lookupEnumByTag(Some(qname), tag, ns0, root))(_ => visit(exp, tenv0))
          }

        case NamedAst.Expression.Unit(loc) => ResolvedAst.Expression.Unit(loc).toSuccess

        case NamedAst.Expression.Null(tvar, loc) => ResolvedAst.Expression.Null(tvar, loc).toSuccess

        case NamedAst.Expression.True(loc) => ResolvedAst.Expression.True(loc).toSuccess

        case NamedAst.Expression.False(loc) => ResolvedAst.Expression.False(loc).toSuccess

        case NamedAst.Expression.Char(lit, loc) => ResolvedAst.Expression.Char(lit, loc).toSuccess

        case NamedAst.Expression.Float32(lit, loc) => ResolvedAst.Expression.Float32(lit, loc).toSuccess

        case NamedAst.Expression.Float64(lit, loc) => ResolvedAst.Expression.Float64(lit, loc).toSuccess

        case NamedAst.Expression.Int8(lit, loc) => ResolvedAst.Expression.Int8(lit, loc).toSuccess

        case NamedAst.Expression.Int16(lit, loc) => ResolvedAst.Expression.Int16(lit, loc).toSuccess

        case NamedAst.Expression.Int32(lit, loc) => ResolvedAst.Expression.Int32(lit, loc).toSuccess

        case NamedAst.Expression.Int64(lit, loc) => ResolvedAst.Expression.Int64(lit, loc).toSuccess

        case NamedAst.Expression.BigInt(lit, loc) => ResolvedAst.Expression.BigInt(lit, loc).toSuccess

        case NamedAst.Expression.Str(lit, loc) => ResolvedAst.Expression.Str(lit, loc).toSuccess

        case NamedAst.Expression.Default(loc) => ResolvedAst.Expression.Default(Type.freshVar(Kind.Star),loc).toSuccess

        case NamedAst.Expression.Apply(exp@NamedAst.Expression.DefSig(qname, _, innerLoc), exps, outerLoc) =>

          (lookupDef(qname, ns0, root), lookupSig(qname, ns0, sigs)) match {
            // Case 1: both def and sig found
            case (Validation.Success(defn), Validation.Success(sig)) =>
              ResolutionError.AmbiguousName(qname, ns0, List(defn.loc, sig.loc), loc).toFailure
            // Case 2: only def found
            case (Validation.Success(defn), Validation.Failure(_)) =>
              if (defn.fparams.length == exps.length) {
                // Case 1: Hooray! We can call the function directly.
                for {
                  es <- traverse(exps)(visit(_, tenv0))
                } yield {
                  val base = ResolvedAst.Expression.Def(defn.sym, Type.freshVar(Kind.Star), innerLoc)
                  ResolvedAst.Expression.Apply(base, es, Type.freshVar(Kind.Star), Type.freshVar(Kind.Bool), outerLoc)
                }
              } else {
                // Case 2: We have to curry. (See below).
                visitApply(exp, exps, outerLoc)
              }
            // Case 3: only sig found
            case (Validation.Failure(_), Validation.Success(sig)) =>
              if (sig.fparams.length == exps.length) {
                // Case 1: Hooray! We can call the function directly.
                for {
                  es <- traverse(exps)(visit(_, tenv0))
                } yield {
                  val base = ResolvedAst.Expression.Sig(sig.sym, Type.freshVar(Kind.Star), innerLoc)
                  ResolvedAst.Expression.Apply(base, es, Type.freshVar(Kind.Star), Type.freshVar(Kind.Bool), outerLoc)
                }
              } else {
                // Case 2: We have to curry. (See below).
                visitApply(exp, exps, outerLoc)
              }
            // Case 4: neither def nor sig found
            case (Validation.Failure(defErrs), Validation.Failure(sigErrs)) =>
              Validation.Failure(defErrs ++ sigErrs) // MATT handle the different errors better; inaccessible beats notfound
              // MATT clean up this logic
          }

        case NamedAst.Expression.Apply(exp, exps, loc) => visitApply(exp, exps, loc)

        case NamedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
          for {
            paramType <- lookupType(fparam.tpe, ns0, root)
            e <- visit(exp, tenv0 + (fparam.sym -> paramType))
            p <- Params.resolve(fparam, ns0, root)
          } yield ResolvedAst.Expression.Lambda(p, e, tvar, loc)

        case NamedAst.Expression.Nullify(exp, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Nullify(e, loc)

        case NamedAst.Expression.Unary(op, exp, tvar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Unary(op, e, tvar, loc)

        case NamedAst.Expression.Binary(op, exp1, exp2, tvar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Binary(op, e1, e2, tvar, loc)

        case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
            e3 <- visit(exp3, tenv0)
          } yield ResolvedAst.Expression.IfThenElse(e1, e2, e3, loc)

        case NamedAst.Expression.Stm(exp1, exp2, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Stm(e1, e2, loc)

        case NamedAst.Expression.Let(sym, exp1, exp2, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Let(sym, e1, e2, loc)

        case NamedAst.Expression.Match(exp, rules, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.MatchRule(pat, guard, body) =>
              for {
                p <- Patterns.resolve(pat, ns0, root)
                g <- visit(guard, tenv0)
                b <- visit(body, tenv0)
              } yield ResolvedAst.MatchRule(p, g, b)
          }

          for {
            e <- visit(exp, tenv0)
            rs <- rulesVal
          } yield ResolvedAst.Expression.Match(e, rs, loc)

        case NamedAst.Expression.NullMatch(exps, rules, loc) =>
          val expsVal = traverse(exps)(visit(_, tenv0))
          val rulesVal = traverse(rules) {
            case NamedAst.NullRule(pat0, exp0) =>
              val p = pat0.map {
                case NamedAst.NullPattern.Wild(loc) => ResolvedAst.NullPattern.Wild(loc)
                case NamedAst.NullPattern.Var(sym, loc) => ResolvedAst.NullPattern.Var(sym, loc)
                case NamedAst.NullPattern.Null(loc) => ResolvedAst.NullPattern.Null(loc)
              }
              mapN(visit(exp0, tenv0)) {
                case e => ResolvedAst.NullRule(p, e)
              }
          }
          mapN(expsVal, rulesVal) {
            case (es, rs) => ResolvedAst.Expression.NullMatch(es, rs, loc)
          }

        case NamedAst.Expression.Tag(enum, tag, expOpt, tvar, loc) => expOpt match {
          case None =>
            // Case 1: The tag has does not have an expression.
            // Either it is implicitly Unit or the tag is used as a function.

            // Lookup the enum to determine the type of the tag.
            lookupEnumByTag(enum, tag, ns0, root) map {
              case decl =>
                // Retrieve the relevant case.
                val caze = decl.cases(tag.name)

                // Check if the tag value has Unit type.
                if (isUnitType(caze.tpe)) {
                  // Case 1.1: The tag value has Unit type. Construct the Unit expression.
                  val e = ResolvedAst.Expression.Unit(loc)
                  ResolvedAst.Expression.Tag(decl.sym, tag.name, e, tvar, loc)
                } else {
                  // Case 1.2: The tag has a non-Unit type. Hence the tag is used as a function.
                  // If the tag is `Some` we construct the lambda: x -> Some(x).

                  // Construct a fresh symbol for the formal parameter.
                  val freshVar = Symbol.freshVarSym("x")

                  // Construct the formal parameter for the fresh symbol.
                  val freshParam = ResolvedAst.FormalParam(freshVar, Ast.Modifiers.Empty, Type.freshVar(Kind.Star), loc)

                  // Construct a variable expression for the fresh symbol.
                  val varExp = ResolvedAst.Expression.Var(freshVar, freshVar.tvar, loc)

                  // Construct the tag expression on the fresh symbol expression.
                  val tagExp = ResolvedAst.Expression.Tag(decl.sym, caze.tag.name, varExp, Type.freshVar(Kind.Star), loc)

                  // Assemble the lambda expressions.
                  ResolvedAst.Expression.Lambda(freshParam, tagExp, Type.freshVar(Kind.Star), loc)
                }
            }
          case Some(exp) =>
            // Case 2: The tag has an expression. Perform resolution on it.
            for {
              d <- lookupEnumByTag(enum, tag, ns0, root)
              e <- visit(exp, tenv0)
            } yield ResolvedAst.Expression.Tag(d.sym, tag.name, e, tvar, loc)
        }

        case NamedAst.Expression.Tuple(elms, loc) =>
          for {
            es <- traverse(elms)(e => visit(e, tenv0))
          } yield ResolvedAst.Expression.Tuple(es, loc)

        case NamedAst.Expression.RecordEmpty(tvar, loc) =>
          ResolvedAst.Expression.RecordEmpty(tvar, loc).toSuccess

        case NamedAst.Expression.RecordSelect(base, label, tvar, loc) =>
          for {
            b <- visit(base, tenv0)
          } yield ResolvedAst.Expression.RecordSelect(b, label.name, tvar, loc)

        case NamedAst.Expression.RecordExtend(label, value, rest, tvar, loc) =>
          for {
            v <- visit(value, tenv0)
            r <- visit(rest, tenv0)
          } yield ResolvedAst.Expression.RecordExtend(label.name, v, r, tvar, loc)

        case NamedAst.Expression.RecordRestrict(label, rest, tvar, loc) =>
          for {
            r <- visit(rest, tenv0)
          } yield ResolvedAst.Expression.RecordRestrict(label.name, r, tvar, loc)

        case NamedAst.Expression.ArrayLit(elms, tvar, loc) =>
          for {
            es <- traverse(elms)(e => visit(e, tenv0))
          } yield ResolvedAst.Expression.ArrayLit(es, tvar, loc)

        case NamedAst.Expression.ArrayNew(elm, len, tvar, loc) =>
          for {
            e <- visit(elm, tenv0)
            ln <- visit(len, tenv0)
          } yield ResolvedAst.Expression.ArrayNew(e, ln, tvar, loc)

        case NamedAst.Expression.ArrayLoad(base, index, tvar, loc) =>
          for {
            b <- visit(base, tenv0)
            i <- visit(index, tenv0)
          } yield ResolvedAst.Expression.ArrayLoad(b, i, tvar, loc)

        case NamedAst.Expression.ArrayStore(base, index, elm, loc) =>
          for {
            b <- visit(base, tenv0)
            i <- visit(index, tenv0)
            e <- visit(elm, tenv0)
          } yield ResolvedAst.Expression.ArrayStore(b, i, e, loc)

        case NamedAst.Expression.ArrayLength(base, loc) =>
          for {
            b <- visit(base, tenv0)
          } yield ResolvedAst.Expression.ArrayLength(b, loc)

        case NamedAst.Expression.ArraySlice(base, startIndex, endIndex, loc) =>
          for {
            b <- visit(base, tenv0)
            i1 <- visit(startIndex, tenv0)
            i2 <- visit(endIndex, tenv0)
          } yield ResolvedAst.Expression.ArraySlice(b, i1, i2, loc)

        case NamedAst.Expression.Ref(exp, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Ref(e, loc)

        case NamedAst.Expression.Deref(exp, tvar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Deref(e, tvar, loc)

        case NamedAst.Expression.Assign(exp1, exp2, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Assign(e1, e2, loc)

        case NamedAst.Expression.Existential(fparam, exp, loc) =>
          for {
            fp <- Params.resolve(fparam, ns0, root)
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Existential(fp, e, loc)

        case NamedAst.Expression.Universal(fparam, exp, loc) =>
          for {
            fp <- Params.resolve(fparam, ns0, root)
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Universal(fp, e, loc)

        case NamedAst.Expression.Ascribe(exp, expectedType, expectedEff, tvar, loc) =>
          val expectedTypVal = expectedType match {
            case None => (None: Option[Type]).toSuccess
            case Some(t) => mapN(lookupType(t, ns0, root))(x => Some(x))
          }
          val expectedEffVal = expectedEff match {
            case None => (None: Option[Type]).toSuccess
            case Some(f) => mapN(lookupType(f, ns0, root))(x => Some(x))
          }

          for {
            e <- visit(exp, tenv0)
            t <- expectedTypVal
            f <- expectedEffVal
            _ <- t.map(checkProperType(_, loc)).getOrElse(().toSuccess)
            _ <- f.map(checkEffectType(_, loc)).getOrElse(().toSuccess)
          } yield ResolvedAst.Expression.Ascribe(e, t, f, tvar, loc)

        case NamedAst.Expression.Cast(exp, declaredType, declaredEff, tvar, loc) =>

          val declaredTypVal = declaredType match {
            case None => (None: Option[Type]).toSuccess
            case Some(t) => mapN(lookupType(t, ns0, root))(x => Some(x))
          }
          val declaredEffVal = declaredEff match {
            case None => (None: Option[Type]).toSuccess
            case Some(f) => mapN(lookupType(f, ns0, root))(x => Some(x))
          }

          for {
            e <- visit(exp, tenv0)
            t <- declaredTypVal
            f <- declaredEffVal
            _ <- t.map(checkProperType(_, loc)).getOrElse(().toSuccess)
            _ <- f.map(checkEffectType(_, loc)).getOrElse(().toSuccess)
          } yield ResolvedAst.Expression.Cast(e, t, f, tvar, loc)

        case NamedAst.Expression.TryCatch(exp, rules, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.CatchRule(sym, clazz, body) =>
              val exceptionType = Type.mkNative(clazz)
              visit(body, tenv0 + (sym -> exceptionType)) map {
                case b => ResolvedAst.CatchRule(sym, clazz, b)
              }
          }

          for {
            e <- visit(exp, tenv0)
            rs <- rulesVal
          } yield ResolvedAst.Expression.TryCatch(e, rs, loc)

        case NamedAst.Expression.InvokeConstructor(className, args, sig, loc) =>
          val argsVal = traverse(args)(visit(_, tenv0))
          val sigVal = traverse(sig)(lookupType(_, ns0, root))
          flatMapN(sigVal, argsVal) {
            case (ts, as) =>
              mapN(lookupJvmConstructor(className, ts, loc)) {
                case constructor => ResolvedAst.Expression.InvokeConstructor(constructor, as, loc)
              }
          }

        case NamedAst.Expression.InvokeMethod(className, methodName, exp, args, sig, loc) =>
          val expVal = visit(exp, tenv0)
          val argsVal = traverse(args)(visit(_, tenv0))
          val sigVal = traverse(sig)(lookupType(_, ns0, root))
          flatMapN(sigVal, expVal, argsVal) {
            case (ts, e, as) =>
              mapN(lookupJvmMethod(className, methodName, ts, static = false, loc)) {
                case method => ResolvedAst.Expression.InvokeMethod(method, e, as, loc)
              }
          }

        case NamedAst.Expression.InvokeStaticMethod(className, methodName, args, sig, loc) =>
          val argsVal = traverse(args)(visit(_, tenv0))
          val sigVal = traverse(sig)(lookupType(_, ns0, root))
          flatMapN(sigVal, argsVal) {
            case (ts, as) =>
              mapN(lookupJvmMethod(className, methodName, ts, static = true, loc)) {
                case method => ResolvedAst.Expression.InvokeStaticMethod(method, as, loc)
              }
          }

        case NamedAst.Expression.GetField(className, fieldName, exp, loc) =>
          mapN(lookupJvmField(className, fieldName, static = false, loc), visit(exp, tenv0)) {
            case (field, e) => ResolvedAst.Expression.GetField(field, e, loc)
          }

        case NamedAst.Expression.PutField(className, fieldName, exp1, exp2, loc) =>
          mapN(lookupJvmField(className, fieldName, static = false, loc), visit(exp1, tenv0), visit(exp2, tenv0)) {
            case (field, e1, e2) => ResolvedAst.Expression.PutField(field, e1, e2, loc)
          }

        case NamedAst.Expression.GetStaticField(className, fieldName, loc) =>
          mapN(lookupJvmField(className, fieldName, static = true, loc)) {
            case field => ResolvedAst.Expression.GetStaticField(field, loc)
          }

        case NamedAst.Expression.PutStaticField(className, fieldName, exp, loc) =>
          mapN(lookupJvmField(className, fieldName, static = true, loc), visit(exp, tenv0)) {
            case (field, e) => ResolvedAst.Expression.PutStaticField(field, e, loc)
          }

        case NamedAst.Expression.NewChannel(exp, tpe, loc) =>
          for {
            t <- lookupType(tpe, ns0, root)
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.NewChannel(e, t, loc)

        case NamedAst.Expression.GetChannel(exp, tvar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.GetChannel(e, tvar, loc)

        case NamedAst.Expression.PutChannel(exp1, exp2, tvar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.PutChannel(e1, e2, tvar, loc)

        case NamedAst.Expression.SelectChannel(rules, default, tvar, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.SelectChannelRule(sym, chan, body) =>
              for {
                c <- visit(chan, tenv0)
                b <- visit(body, tenv0)
              } yield ResolvedAst.SelectChannelRule(sym, c, b)
          }

          val defaultVal = default match {
            case Some(exp) =>
              for {
                e <- visit(exp, tenv0)
              } yield Some(e)
            case None => None.toSuccess
          }

          for {
            rs <- rulesVal
            d <- defaultVal
          } yield ResolvedAst.Expression.SelectChannel(rs, d, tvar, loc)

        case NamedAst.Expression.Spawn(exp, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Spawn(e, loc)

        case NamedAst.Expression.Lazy(exp, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Lazy(e, loc)

        case NamedAst.Expression.Force(exp, tvar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Force(e, tvar, loc)

        case NamedAst.Expression.FixpointConstraintSet(cs0, tvar, loc) =>
          for {
            cs <- traverse(cs0)(Constraints.resolve(_, tenv0, ns0, root, sigs))
          } yield ResolvedAst.Expression.FixpointConstraintSet(cs, tvar, loc)

        case NamedAst.Expression.FixpointCompose(exp1, exp2, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.FixpointCompose(e1, e2, loc)

        case NamedAst.Expression.FixpointSolve(exp, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.FixpointSolve(e, loc)

        case NamedAst.Expression.FixpointProject(ident, exp, tvar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.FixpointProject(ident.name, e, tvar, loc)

        case NamedAst.Expression.FixpointEntails(exp1, exp2, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.FixpointEntails(e1, e2, loc)

        case NamedAst.Expression.FixpointFold(ident, exp1, exp2, exp3, tvar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
            e3 <- visit(exp3, tenv0)
          } yield ResolvedAst.Expression.FixpointFold(ident.name, e1, e2, e3, tvar, loc)
      }

      visit(exp0, Map.empty)
    }

  }

  object Patterns {

    /**
      * Performs name resolution on the given pattern `pat0` in the namespace `ns0`.
      */
    def resolve(pat0: NamedAst.Pattern, ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.Pattern, ResolutionError] = {

      def visit(p0: NamedAst.Pattern): Validation[ResolvedAst.Pattern, ResolutionError] = p0 match {
        case NamedAst.Pattern.Wild(tvar, loc) => ResolvedAst.Pattern.Wild(tvar, loc).toSuccess

        case NamedAst.Pattern.Var(sym, tvar, loc) => ResolvedAst.Pattern.Var(sym, tvar, loc).toSuccess

        case NamedAst.Pattern.Unit(loc) => ResolvedAst.Pattern.Unit(loc).toSuccess

        case NamedAst.Pattern.True(loc) => ResolvedAst.Pattern.True(loc).toSuccess

        case NamedAst.Pattern.False(loc) => ResolvedAst.Pattern.False(loc).toSuccess

        case NamedAst.Pattern.Char(lit, loc) => ResolvedAst.Pattern.Char(lit, loc).toSuccess

        case NamedAst.Pattern.Float32(lit, loc) => ResolvedAst.Pattern.Float32(lit, loc).toSuccess

        case NamedAst.Pattern.Float64(lit, loc) => ResolvedAst.Pattern.Float64(lit, loc).toSuccess

        case NamedAst.Pattern.Int8(lit, loc) => ResolvedAst.Pattern.Int8(lit, loc).toSuccess

        case NamedAst.Pattern.Int16(lit, loc) => ResolvedAst.Pattern.Int16(lit, loc).toSuccess

        case NamedAst.Pattern.Int32(lit, loc) => ResolvedAst.Pattern.Int32(lit, loc).toSuccess

        case NamedAst.Pattern.Int64(lit, loc) => ResolvedAst.Pattern.Int64(lit, loc).toSuccess

        case NamedAst.Pattern.BigInt(lit, loc) => ResolvedAst.Pattern.BigInt(lit, loc).toSuccess

        case NamedAst.Pattern.Str(lit, loc) => ResolvedAst.Pattern.Str(lit, loc).toSuccess

        case NamedAst.Pattern.Tag(enum, tag, pat, tvar, loc) =>
          for {
            d <- lookupEnumByTag(enum, tag, ns0, root)
            p <- visit(pat)
          } yield ResolvedAst.Pattern.Tag(d.sym, tag.name, p, tvar, loc)

        case NamedAst.Pattern.Tuple(elms, loc) =>
          for {
            es <- traverse(elms)(visit)
          } yield ResolvedAst.Pattern.Tuple(es, loc)

        case NamedAst.Pattern.Array(elms, tvar, loc) =>
          for {
            es <- traverse(elms)(visit)
          } yield ResolvedAst.Pattern.Array(es, tvar, loc)

        case NamedAst.Pattern.ArrayTailSpread(elms, sym, tvar, loc) =>
          for {
            es <- traverse(elms)(visit)
          } yield ResolvedAst.Pattern.ArrayTailSpread(es, sym, tvar, loc)

        case NamedAst.Pattern.ArrayHeadSpread(sym, elms, tvar, loc) =>
          for {
            es <- traverse(elms)(visit)
          } yield ResolvedAst.Pattern.ArrayHeadSpread(sym, es, tvar, loc)
      }

      visit(pat0)
    }

  }

  object Predicates {

    object Head {
      /**
        * Performs name resolution on the given head predicate `h0` in the given namespace `ns0`.
        */
      def resolve(h0: NamedAst.Predicate.Head, tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Predicate.Head, ResolutionError] = h0 match {
        case NamedAst.Predicate.Head.Atom(ident, den, terms, tvar, loc) =>
          for {
            ts <- traverse(terms)(t => Expressions.resolve(t, tenv0, ns0, root, sigs))
          } yield ResolvedAst.Predicate.Head.Atom(ident.name, den, ts, tvar, loc)

        case NamedAst.Predicate.Head.Union(exp, tvar, loc) =>
          for {
            e <- Expressions.resolve(exp, tenv0, ns0, root, sigs)
          } yield ResolvedAst.Predicate.Head.Union(e, tvar, loc)
      }
    }

    object Body {
      /**
        * Performs name resolution on the given body predicate `b0` in the given namespace `ns0`.
        */
      def resolve(b0: NamedAst.Predicate.Body, tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Predicate.Body, ResolutionError] = b0 match {
        case NamedAst.Predicate.Body.Atom(ident, den, polarity, terms, tvar, loc) =>
          for {
            ts <- traverse(terms)(t => Patterns.resolve(t, ns0, root))
          } yield ResolvedAst.Predicate.Body.Atom(ident.name, den, polarity, ts, tvar, loc)

        case NamedAst.Predicate.Body.Guard(exp, loc) =>
          for {
            e <- Expressions.resolve(exp, tenv0, ns0, root, sigs)
          } yield ResolvedAst.Predicate.Body.Guard(e, loc)
      }
    }

  }

  object Properties {

    /**
      * Performs name resolution on each of the given `properties` in the given namespace `ns0`.
      */
    def resolve(properties: List[NamedAst.Property], ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[List[ResolvedAst.Property], ResolutionError] = {
      traverse(properties)(p => resolve(p, ns0, root, sigs))
    }

    /**
      * Performs name resolution on the given property `p0` in the given namespace `ns0`.
      */
    def resolve(p0: NamedAst.Property, ns0: Name.NName, root: NamedAst.Root, sigs: SigLookup)(implicit flix: Flix): Validation[ResolvedAst.Property, ResolutionError] = {
      for {
        e <- Expressions.resolve(p0.exp, Map.empty, ns0, root, sigs)
      } yield ResolvedAst.Property(p0.law, p0.defn, e, p0.loc)
    }

  }

  object Params {

    /**
      * Performs name resolution on the given constraint parameter `cparam0` in the given namespace `ns0`.
      */
    def resolve(cparam0: NamedAst.ConstraintParam, ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.ConstraintParam, ResolutionError] = cparam0 match {
      case NamedAst.ConstraintParam.HeadParam(sym, tpe, loc) => ResolvedAst.ConstraintParam.HeadParam(sym, tpe, loc).toSuccess
      case NamedAst.ConstraintParam.RuleParam(sym, tpe, loc) => ResolvedAst.ConstraintParam.RuleParam(sym, tpe, loc).toSuccess
    }

    /**
      * Performs name resolution on the given formal parameter `fparam0` in the given namespace `ns0`.
      */
    def resolve(fparam0: NamedAst.FormalParam, ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.FormalParam, ResolutionError] = {
      for {
        t <- lookupType(fparam0.tpe, ns0, root)
        _ <- checkProperType(t, fparam0.loc)
      } yield ResolvedAst.FormalParam(fparam0.sym, fparam0.mod, t, fparam0.loc)
    }

    /**
      * Performs name resolution on the given type parameter `tparam0` in the given namespace `ns0`.
      */
    def resolve(tparam0: NamedAst.TypeParam, ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.TypeParam, ResolutionError] = {
      ResolvedAst.TypeParam(tparam0.name, tparam0.tpe, tparam0.loc).toSuccess
    }

  }

  /**
    * Performs name resolution on the given formal parameters `fparams0`.
    */
  def resolveFormalParams(fparams0: List[NamedAst.FormalParam], ns0: Name.NName, root: NamedAst.Root): Validation[List[ResolvedAst.FormalParam], ResolutionError] = {
    traverse(fparams0)(fparam => Params.resolve(fparam, ns0, root))
  }

  /**
    * Performs name resolution on the given type parameters `tparams0`.
    */
  def resolveTypeParams(tparams0: List[NamedAst.TypeParam], ns0: Name.NName, root: NamedAst.Root): Validation[List[ResolvedAst.TypeParam], ResolutionError] =
    traverse(tparams0)(tparam => Params.resolve(tparam, ns0, root))

  /**
    * Performs name resolution on the given scheme `sc0`.
    */
  def resolveScheme(sc0: NamedAst.Scheme, ns0: Name.NName, root: NamedAst.Root): Validation[Scheme, ResolutionError] = {
    for {
      base <- lookupType(sc0.base, ns0, root)
    } yield Scheme(sc0.quantifiers, base)
  }

  /**
    * Finds the definition with the qualified name `qname` in the namespace `ns0`.
    */
  def lookupDef(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Def, ResolutionError] = {
    val defOpt = tryLookupDef(qname, ns0, root)

    defOpt match {
      case None => ResolutionError.UndefinedName(qname, ns0, qname.loc).toFailure
      case Some(d) => getDefIfAccessible(d, ns0, qname.loc)
    }
  }

  /**
    * Tries to find a def with the qualified name `qname` in the namespace `ns0`.
    */
  def tryLookupDef(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.Def] = {
    // Check whether the name is fully-qualified.
    if (qname.isUnqualified) {
      // Case 1: Unqualified name. Lookup in the current namespace.
      val defnOpt = root.defs.getOrElse(ns0, Map.empty).get(qname.ident.name)

      defnOpt match {
        case Some(defn) =>
          // Case 1.2: Found in the current namespace.
          Some(defn)
        case None =>
          // Case 1.1: Try the global namespace.
          root.defs.getOrElse(Name.RootNS, Map.empty).get(qname.ident.name)
      }
    } else {
      // Case 2: Qualified. Lookup in the given namespace.
      root.defs.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Finds the signature with the qualified name `qname` in the namespace `ns0`.
    */
  def lookupSig(qname: Name.QName, ns0: Name.NName, sigs: SigLookup): Validation[NamedAst.Sig, ResolutionError] = {
    val sigOpt = tryLookupSig(qname, ns0, sigs)

    sigOpt match {
      case None => ResolutionError.UndefinedName(qname, ns0, qname.loc).toFailure
      case Some(s) => getSigIfAccessible(s, ns0, qname.loc)
    }
  }

  /**
    * Tries to find a sig with the qualified name `qname` in the namespace `ns0`.
    */
  def tryLookupSig(qname: Name.QName, ns0: Name.NName, sigs: SigLookup): Option[NamedAst.Sig] = {
    // Check whether the name is fully-qualified.
    if (qname.isUnqualified) {
      // Case 1: Unqualified name. Lookup in the current namespace.
      val sigOpt = sigs.getOrElse(ns0, Map.empty).get(qname.ident.name)

      sigOpt match {
        case Some(sig) =>
          // Case 1.2: Found in the current namespace.
          Some(sig)
        case None =>
          // Case 1.1: Try the global namespace.
          sigs.getOrElse(Name.RootNS, Map.empty).get(qname.ident.name)
      }
    } else {
      // Case 2: Qualified. Lookup in the given namespace.
      sigs.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Builds a nested map of namespace -> name -> signature from the given class namespace map..
    */
  def buildSigLookup(classes: Map[Name.NName, Map[String, NamedAst.Class]]): SigLookup = {
    def flatMapToSigs(classes: Map[String, NamedAst.Class]): Map[String, NamedAst.Sig] = {
      classes.flatMap {
        case (_, clazz) => clazz.signatures.map(sig => (sig.sym.name, sig))
      }
    }
    classes.view.mapValues(flatMapToSigs).toMap
  }

  /**
    * Finds the enum that maches the given qualified name `qname` and `tag` in the namespace `ns0`.
    */
  def lookupEnumByTag(qnameOpt: Option[Name.QName], tag: Name.Ident, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Enum, ResolutionError] = {
    // Determine whether the name is qualified.
    qnameOpt match {
      case None =>
        // Case 1: The name is unqualified.

        // Find all matching enums in the current namespace.
        val namespaceMatches = mutable.Set.empty[NamedAst.Enum]
        for ((enumName, decl) <- root.enums.getOrElse(ns0, Map.empty[String, NamedAst.Enum])) {
          for ((tagName, caze) <- decl.cases) {
            if (tag.name == tagName) {
              namespaceMatches += decl
            }
          }
        }

        // Case 1.1.1: Exact match found in the namespace.
        if (namespaceMatches.size == 1) {
          return getEnumIfAccessible(namespaceMatches.head, ns0, tag.loc)
        }

        // Case 1.1.2: Multiple matches found in the namespace.
        if (namespaceMatches.size > 1) {
          val locs = namespaceMatches.map(_.sym.loc).toList.sorted
          return ResolutionError.AmbiguousTag(tag.name, ns0, locs, tag.loc).toFailure
        }

        // Find all matching enums in the root namespace.
        val globalMatches = mutable.Set.empty[NamedAst.Enum]
        for (decls <- root.enums.get(Name.RootNS)) {
          for ((enumName, decl) <- decls) {
            for ((tagName, caze) <- decl.cases) {
              if (tag.name == tagName) {
                globalMatches += decl
              }
            }
          }
        }

        // Case 1.2.1: Exact match found in the root namespace.
        if (globalMatches.size == 1) {
          return getEnumIfAccessible(globalMatches.head, ns0, tag.loc)
        }

        // Case 1.2.2: Multiple matches found in the root namespace.
        if (globalMatches.size > 1) {
          val locs = globalMatches.map(_.sym.loc).toList.sorted
          return ResolutionError.AmbiguousTag(tag.name, ns0, locs, tag.loc).toFailure
        }

        // Case 1.2.3: No match found.
        ResolutionError.UndefinedTag(tag.name, ns0, tag.loc).toFailure

      case Some(qname) =>
        // Case 2: The name is qualified.

        // Determine where to search for the enum.
        val enumsInNS = if (qname.isUnqualified) {
          // The name is unqualified (e.g. Option.None) so search in the current namespace.
          root.enums.getOrElse(ns0, Map.empty[String, NamedAst.Enum])
        } else {
          // The name is qualified (e.g. Foo/Bar/Baz.Qux) so search in the Foo/Bar/Baz namespace.
          root.enums.getOrElse(qname.namespace, Map.empty[String, NamedAst.Enum])
        }

        // Lookup the enum declaration.
        enumsInNS.get(qname.ident.name) match {
          case None =>
            // Case 2.1: The enum does not exist.
            ResolutionError.UndefinedType(qname, ns0, qname.loc).toFailure
          case Some(enumDecl) =>
            // Case 2.2: Enum declaration found. Look for the tag.
            for ((tagName, caze) <- enumDecl.cases) {
              if (tag.name == tagName) {
                // Case 2.2.1: Tag found.
                return getEnumIfAccessible(enumDecl, ns0, tag.loc)
              }
            }

            // Case 2.2.2: No match found.
            ResolutionError.UndefinedTag(tag.name, ns0, tag.loc).toFailure
        }
    }

  }

  /**
    * Returns `true` iff the given type `tpe0` is the Unit type.
    */
  def isUnitType(tpe: NamedAst.Type): Boolean = tpe match {
    case NamedAst.Type.Unit(loc) => true
    case _ => false
  }

  /**
    * Resolves the given type `tpe0` in the given namespace `ns0`.
    */
  def lookupType(tpe0: NamedAst.Type, ns0: Name.NName, root: NamedAst.Root)(implicit recursionDepth: Int = 0): Validation[Type, ResolutionError] = tpe0 match {
    case NamedAst.Type.Var(tvar, loc) => tvar.toSuccess

    case NamedAst.Type.Unit(loc) => Type.Unit.toSuccess

    case NamedAst.Type.Ambiguous(qname, loc) if qname.isUnqualified => qname.ident.name match {
      // Basic Types
      case "Unit" => Type.Unit.toSuccess
      case "Bool" => Type.Bool.toSuccess
      case "Char" => Type.Char.toSuccess
      case "Float" => Type.Float64.toSuccess
      case "Float32" => Type.Float32.toSuccess
      case "Float64" => Type.Float64.toSuccess
      case "Int" => Type.Int32.toSuccess
      case "Int8" => Type.Int8.toSuccess
      case "Int16" => Type.Int16.toSuccess
      case "Int32" => Type.Int32.toSuccess
      case "Int64" => Type.Int64.toSuccess
      case "BigInt" => Type.BigInt.toSuccess
      case "Str" => Type.Str.toSuccess
      case "String" => Type.Str.toSuccess
      case "Array" => Type.Array.toSuccess
      case "Channel" => Type.Channel.toSuccess
      case "Lazy" => Type.Lazy.toSuccess
      case "Ref" => Type.Ref.toSuccess

      // Disambiguate type.
      case typeName =>
        (lookupEnum(qname, ns0, root), lookupTypeAlias(qname, ns0, root)) match {
          // Case 1: Not Found.
          case (None, None) => ResolutionError.UndefinedType(qname, ns0, loc).toFailure

          // Case 2: Enum.
          case (Some(enum), None) => getEnumTypeIfAccessible(enum, ns0, ns0.loc)

          // Case 3: TypeAlias.
          case (None, Some(typealias)) => getTypeAliasIfAccessible(typealias, ns0, root, ns0.loc)

          // Case 4: Errors.
          case (x, y) =>
            val loc1 = x.map(_.loc)
            val loc2 = y.map(_.loc)
            val locs = List(loc1, loc2).flatten
            ResolutionError.AmbiguousType(typeName, ns0, locs, loc).toFailure
        }
    }

    case NamedAst.Type.Ambiguous(qname, loc) if qname.isQualified =>
      // Disambiguate type.
      (lookupEnum(qname, ns0, root), lookupTypeAlias(qname, ns0, root)) match {
        case (None, None) => ResolutionError.UndefinedType(qname, ns0, loc).toFailure
        case (Some(enumDecl), None) => getEnumTypeIfAccessible(enumDecl, ns0, loc)
        case (None, Some(typeAliasDecl)) => getTypeAliasIfAccessible(typeAliasDecl, ns0, root, loc)
        case (Some(enumDecl), Some(typeAliasDecl)) =>
          val locs = enumDecl.loc :: typeAliasDecl.loc :: Nil
          ResolutionError.AmbiguousType(qname.ident.name, ns0, locs, loc).toFailure
      }

    case NamedAst.Type.Enum(sym, kind) =>
      Type.mkEnum(sym, kind).toSuccess

    case NamedAst.Type.Tuple(elms0, loc) =>
      for {
        elms <- traverse(elms0)(tpe => lookupType(tpe, ns0, root))
        tup <- mkTuple(elms, loc)
      } yield tup

    case NamedAst.Type.RecordEmpty(loc) =>
      Type.RecordEmpty.toSuccess

    case NamedAst.Type.RecordExtend(label, value, rest, loc) =>
      for {
        v <- lookupType(value, ns0, root)
        r <- lookupType(rest, ns0, root)
        rec <- mkRecordExtend(label.name, v, r, loc)
      } yield rec

    case NamedAst.Type.SchemaEmpty(loc) =>
      Type.SchemaEmpty.toSuccess

    case NamedAst.Type.SchemaExtendWithAlias(qname, targs, rest, loc) =>
      // Lookup the type alias.
      lookupTypeAlias(qname, ns0, root) match {
        case None =>
          // Case 1: The type alias was not found. Report an error.
          ResolutionError.UndefinedName(qname, ns0, loc).toFailure
        case Some(typealias) =>
          // Case 2: The type alias was found. Use it.
          for {
            t <- getTypeAliasIfAccessible(typealias, ns0, root, loc)
            ts <- traverse(targs)(lookupType(_, ns0, root))
            r <- lookupType(rest, ns0, root)
            app <- mkApply(t, ts, loc)
            tpe <- Type.simplify(app).toSuccess[Type, ResolutionError]
            schema <- mkSchemaExtend(qname.ident.name, tpe, r, loc)
          } yield schema
      }

    case NamedAst.Type.SchemaExtendWithTypes(ident, den, tpes, rest, loc) =>
      for {
        ts <- traverse(tpes)(lookupType(_, ns0, root))
        r <- lookupType(rest, ns0, root)
        pred <- mkPredicate(den, ts, loc)
        schema <- mkSchemaExtend(ident.name, pred, r, loc)
      } yield schema

    case NamedAst.Type.Relation(tpes, loc) =>
      for {
        ts <- traverse(tpes)(lookupType(_, ns0, root))
        rel <- mkRelation(ts, loc)
      } yield rel

    case NamedAst.Type.Lattice(tpes, loc) =>
      for {
        ts <- traverse(tpes)(lookupType(_, ns0, root))
        lat <- mkLattice(ts, loc)
      } yield lat

    case NamedAst.Type.Native(fqn, loc) =>
      fqn match {
        case "java.math.BigInteger" => Type.BigInt.toSuccess
        case "java.lang.String" => Type.Str.toSuccess
        case _ => lookupJvmClass(fqn, loc) map {
          case clazz => Type.mkNative(clazz)
        }
      }

    case NamedAst.Type.Nullable(tpe, nullity, loc) =>
      for {
        t <- lookupType(tpe, ns0, root)
        n <- lookupType(nullity, ns0, root)
        res <- mkNullable(t, n, loc)
      } yield res

    case NamedAst.Type.Arrow(tparams0, eff0, tresult0, loc) =>
      for {
        tparams <- traverse(tparams0)(lookupType(_, ns0, root))
        tresult <- lookupType(tresult0, ns0, root)
        eff <- lookupType(eff0, ns0, root)
      } yield Type.mkUncurriedArrowWithEffect(tparams, eff, tresult) // TODO lift this once Type.Arrow effect is moved

    case NamedAst.Type.Apply(base0, targ0, loc) =>
      for {
        tpe1 <- lookupType(base0, ns0, root)
        tpe2 <- lookupType(targ0, ns0, root)
        app <- mkApply(tpe1, tpe2, loc)
      } yield Type.simplify(app)

    case NamedAst.Type.True(loc) =>
      Type.True.toSuccess

    case NamedAst.Type.False(loc) =>
      Type.False.toSuccess

    case NamedAst.Type.Not(tpe, loc) =>
      flatMapN(lookupType(tpe, ns0, root)) {
        case t => mkNot(t, loc)
      }

    case NamedAst.Type.And(tpe1, tpe2, loc) =>
      flatMapN(lookupType(tpe1, ns0, root), lookupType(tpe2, ns0, root)) {
        case (t1, t2) => mkAnd(t1, t2, loc)
      }

    case NamedAst.Type.Or(tpe1, tpe2, loc) =>
      flatMapN(lookupType(tpe1, ns0, root), lookupType(tpe2, ns0, root)) {
        case (t1, t2) => mkOr(t1, t2, loc)
      }

  }

  /**
    * Optionally returns the enum with the given `name` in the given namespace `ns0`.
    */
  private def lookupEnum(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.Enum] = {
    if (qname.isUnqualified) {
      // Case 1: The name is unqualified. Lookup in the current namespace.
      val enumsInNamespace = root.enums.getOrElse(ns0, Map.empty)
      enumsInNamespace.get(qname.ident.name) orElse {
        // Case 1.1: The name was not found in the current namespace. Try the root namespace.
        val enumsInRootNS = root.enums.getOrElse(Name.RootNS, Map.empty)
        enumsInRootNS.get(qname.ident.name)
      }
    } else {
      // Case 2: The name is qualified. Look it up in its namespace.
      root.enums.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Optionally returns the type alias with the given `name` in the given namespace `ns0`.
    */
  private def lookupTypeAlias(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.TypeAlias] = {
    if (qname.isUnqualified) {
      // Case 1: The name is unqualified. Lookup in the current namespace.
      val typeAliasesInNamespace = root.typealiases.getOrElse(ns0, Map.empty)
      typeAliasesInNamespace.get(qname.ident.name) orElse {
        // Case 1.1: The name was not found in the current namespace. Try the root namespace.
        val typeAliasesInRootNS = root.typealiases.getOrElse(Name.RootNS, Map.empty)
        typeAliasesInRootNS.get(qname.ident.name)
      }
    } else {
      // Case 2: The name is qualified. Look it up in its namespace.
      root.typealiases.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Asserts that the given type is a proper type: that its kind is a subkind of `*`.
    */
  private def checkProperType(tpe: Type, loc: SourceLocation): Validation[Unit, ResolutionError] = {
    if (tpe.kind <:: Kind.Star) {
      ().toSuccess
    } else {
      ResolutionError.IllegalUninhabitedType(tpe, loc).toFailure
    }
  }

  /**
    * Asserts that the given type is an effect type: that its kind is a subkind of `Bool`.
    */
  private def checkEffectType(tpe: Type, loc: SourceLocation): Validation[Unit, ResolutionError] = {
    if (tpe.kind <:: Kind.Bool) {
      ().toSuccess
    } else {
      ResolutionError.IllegalEffect(tpe, loc).toFailure
    }
  }

  /**
    * Successfully returns the given definition `defn0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * A definition `defn0` is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def getDefIfAccessible(defn0: NamedAst.Def, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Def, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (defn0.mod.isPublic)
      return defn0.toSuccess

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = defn0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return defn0.toSuccess

    //
    // The definition is not accessible.
    //
    ResolutionError.InaccessibleDef(defn0.sym, ns0, loc).toFailure
  }

  /**
    * Successfully returns the given signature `sig0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * A signature `sig0` is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def getSigIfAccessible(sig0: NamedAst.Sig, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Sig, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (sig0.mod.isPublic) // TODO use class visibility instead of sig
      return sig0.toSuccess

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = sig0.sym.clazz.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return sig0.toSuccess

    //
    // The definition is not accessible.
    //
    ResolutionError.InaccessibleSig(sig0.sym, ns0, loc).toFailure
  }

  /**
    * Successfully returns the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def getEnumIfAccessible(enum0: NamedAst.Enum, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Enum, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (enum0.mod.isPublic)
      return enum0.toSuccess

    //
    // Check if the enum is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = enum0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return enum0.toSuccess

    //
    // The enum is not accessible.
    //
    ResolutionError.InaccessibleEnum(enum0.sym, ns0, loc).toFailure
  }


  /**
    * Successfully returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  def getEnumTypeIfAccessible(enum0: NamedAst.Enum, ns0: Name.NName, loc: SourceLocation): Validation[Type, ResolutionError] =
    getEnumIfAccessible(enum0, ns0, loc) map {
      case enum => Type.mkEnum(enum.sym, enum0.kind)
    }

  /**
    * Successfully returns the type of the given type alias `alia0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getTypeAliasIfAccessible(alia0: NamedAst.TypeAlias, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation)(implicit recursionDepth: Int): Validation[Type, ResolutionError] = {
    // TODO: We should check if the type alias is accessible.

    ///
    /// Check whether we have hit the recursion limit while unfolding the type alias.
    ///
    if (recursionDepth == RecursionLimit) {
      return ResolutionError.RecursionLimit(alia0.sym, RecursionLimit, alia0.loc).toFailure
    }

    // Retrieve the declaring namespace.
    val declNS = getNS(alia0.sym.namespace)

    // Construct a type lambda for each type parameter.
    mapN(lookupType(alia0.tpe, declNS, root)(recursionDepth + 1)) {
      case base => mkTypeLambda(alia0.tparams, base)
    }
  }

  /**
    * Returns the given type `tpe` wrapped in a type lambda for the given type parameters `tparam`.
    */
  private def mkTypeLambda(tparams: List[NamedAst.TypeParam], tpe: Type): Type =
    tparams.foldRight(tpe) {
      case (tparam, acc) => Type.Lambda(tparam.tpe, acc)
    }


  /**
    * Returns the class reflection object for the given `className`.
    */
  private def lookupJvmClass(className: String, loc: SourceLocation): Validation[Class[_], ResolutionError] = try {
    Class.forName(className).toSuccess
  } catch {
    case ex: ClassNotFoundException => ResolutionError.UndefinedJvmClass(className, loc).toFailure
  }

  /**
    * Returns the constructor reflection object for the given `className` and `signature`.
    */
  private def lookupJvmConstructor(className: String, signature: List[Type], loc: SourceLocation): Validation[Constructor[_], ResolutionError] = {
    // Lookup the class and signature.
    flatMapN(lookupJvmClass(className, loc), lookupSignature(signature, loc)) {
      case (clazz, sig) => try {
        // Lookup the constructor with the appropriate signature.
        clazz.getConstructor(sig: _*).toSuccess
      } catch {
        case ex: ClassNotFoundException => ResolutionError.UndefinedJvmClass(className, loc).toFailure
        case ex: NoSuchMethodException => ResolutionError.UndefinedJvmConstructor(className, sig, clazz.getConstructors.toList, loc).toFailure
      }
    }
  }

  /**
    * Returns the method reflection object for the given `className`, `methodName`, and `signature`.
    */
  private def lookupJvmMethod(className: String, methodName: String, signature: List[Type], static: Boolean, loc: SourceLocation): Validation[Method, ResolutionError] = {
    // Lookup the class and signature.
    flatMapN(lookupJvmClass(className, loc), lookupSignature(signature, loc)) {
      case (clazz, sig) => try {
        // Lookup the method with the appropriate signature.
        val method = clazz.getDeclaredMethod(methodName, sig: _*)

        // Check if the method should be and is static.
        if (static == Modifier.isStatic(method.getModifiers))
          method.toSuccess
        else
          throw new NoSuchMethodException()
      } catch {
        case ex: NoSuchMethodException =>
          val candidateMethods = clazz.getMethods.filter(m => m.getName == methodName).toList
          ResolutionError.UndefinedJvmMethod(className, methodName, static, sig, candidateMethods, loc).toFailure
      }
    }
  }

  /**
    * Returns the field reflection object for the given `className` and `fieldName`.
    */
  private def lookupJvmField(className: String, fieldName: String, static: Boolean, loc: SourceLocation): Validation[Field, ResolutionError] = {
    flatMapN(lookupJvmClass(className, loc)) {
      case clazz => try {
        // Lookup the field.
        val field = clazz.getField(fieldName)

        // Check if the field should be and is static.
        if (static == Modifier.isStatic(field.getModifiers))
          field.toSuccess
        else
          throw new NoSuchFieldException()
      } catch {
        case ex: NoSuchFieldException =>
          val candidateFields = clazz.getFields.toList
          ResolutionError.UndefinedJvmField(className, fieldName, static, candidateFields, loc).toFailure
      }
    }
  }

  /**
    * Performs name resolution on the given `signature`.
    */
  private def lookupSignature(signature: List[Type], loc: SourceLocation): Validation[List[Class[_]], ResolutionError] = {
    traverse(signature)(getJVMType(_, loc))
  }

  /**
    * Returns the JVM type corresponding to the given Flix type `tpe`.
    *
    * A non-primitive Flix type is mapped to java.lang.Object.
    *
    * An array type is mapped to the corresponding array type.
    */
  private def getJVMType(tpe: Type, loc: SourceLocation): Validation[Class[_], ResolutionError] = tpe.typeConstructor match {
    case None =>
      ResolutionError.IllegalType(tpe, loc).toFailure

    case Some(tc) => tc match {
      case TypeConstructor.Unit => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.Bool => classOf[Boolean].toSuccess

      case TypeConstructor.Char => classOf[Char].toSuccess

      case TypeConstructor.Float32 => classOf[Float].toSuccess

      case TypeConstructor.Float64 => classOf[Double].toSuccess

      case TypeConstructor.Int8 => classOf[Byte].toSuccess

      case TypeConstructor.Int16 => classOf[Short].toSuccess

      case TypeConstructor.Int32 => classOf[Int].toSuccess

      case TypeConstructor.Int64 => classOf[Long].toSuccess

      case TypeConstructor.BigInt => Class.forName("java.math.BigInteger").toSuccess

      case TypeConstructor.Str => Class.forName("java.lang.String").toSuccess

      case TypeConstructor.Channel => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.Enum(_, _) => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.Ref => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.Tuple(_) => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.Array =>
        tpe.typeArguments match {
          case elmTyp :: Nil =>
            mapN(getJVMType(elmTyp, loc)) {
              case elmClass =>
                // See: https://stackoverflow.com/questions/1679421/how-to-get-the-array-class-for-a-given-class-in-java
                java.lang.reflect.Array.newInstance(elmClass, 0).getClass
            }
          case _ => ResolutionError.IllegalType(tpe, loc).toFailure
        }

      case TypeConstructor.Native(clazz) => clazz.toSuccess

      case TypeConstructor.RecordEmpty => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.RecordExtend(_) => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.SchemaEmpty => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.SchemaExtend(_) => Class.forName("java.lang.Object").toSuccess

      case _ => ResolutionError.IllegalType(tpe, loc).toFailure
    }
  }

  /**
    * Returns a synthetic namespace obtained from the given sequence of namespace `parts`.
    */
  private def getNS(parts: List[String]): Name.NName = {
    val sp1 = SourcePosition.Unknown
    val sp2 = SourcePosition.Unknown
    val idents = parts.map(s => Name.Ident(sp1, s, sp2))
    Name.NName(sp1, idents, sp2)
  }

  /**
    * Create a well-formed type applying `tpe1` to `tpe2`.
    */
  private def mkApply(tpe1: Type, tpe2: Type, loc: SourceLocation): Validation[Type, ResolutionError] = {
    (tpe1.kind, tpe2.kind)  match {
      case (Kind.Arrow(k1, _), k2) if k2 <:: k1 => Type.Apply(tpe1, tpe2).toSuccess
      case _ => ResolutionError.IllegalTypeApplication(tpe1, tpe2, loc).toFailure
    }
  }

  /**
    * Create a well-formed type applying `tpe` to `args`.
    */
  private def mkApply(tpe: Type, args: List[Type], loc: SourceLocation): Validation[Type, ResolutionError] = {
    Validation.fold(args, tpe)(mkApply(_, _, loc))
  }

  /**
    * Create a well-formed `And` type.
    */
  private def mkAnd(tpe1: Type, tpe2: Type, loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkApply(Type.And, List(tpe1, tpe2), loc)
  }

  /**
    * Create a well-formed `Or` type.
    */
  private def mkOr(tpe1: Type, tpe2: Type, loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkApply(Type.Or, List(tpe1, tpe2), loc)
  }

  /**
    * Create a well-formed `Not` type.
    */
  private def mkNot(tpe: Type, loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkApply(Type.Not, tpe, loc)
  }

  /**
    * Create a well-formed `Nullable` type.
    */
  private def mkNullable(tpe: Type, nullity: Type, loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkApply(Type.Cst(TypeConstructor.Nullable), List(tpe, nullity), loc)
  }

  /**
    * Creates a well-formed `Tuple` type.
    */
  private def mkTuple(tpes: List[Type], loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkApply(Type.Cst(TypeConstructor.Tuple(tpes.length)), tpes, loc)
  }

  /**
    * Creates a well-formed `RecordExtend` type.
    */
  private def mkRecordExtend(label: String, tpe: Type, rest: Type, loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkApply(Type.Cst(TypeConstructor.RecordExtend(label)), List(tpe, rest), loc)
  }

  /**
    * Creates a well-formed `SchemaExtend` type.
    */
  private def mkSchemaExtend(name: String, tpe: Type, rest: Type, loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkApply(Type.Cst(TypeConstructor.SchemaExtend(name)), List(tpe, rest), loc)
  }

  /**
    * Creates a well-formed `Lattice` or `Relation` type.
    */
  private def mkPredicate(den: Ast.Denotation, ts0: List[Type], loc: SourceLocation): Validation[Type, ResolutionError] = {
    val tycon = den match {
      case Denotation.Relational => Type.Relation
      case Denotation.Latticenal => Type.Lattice
    }
    val tsVal = ts0 match {
      case Nil => Type.Unit.toSuccess
      case x :: Nil => x.toSuccess
      case xs => mkTuple(xs, loc)
    }

    flatMapN(tsVal)(mkApply(tycon, _, loc))
  }

  /**
    * Creates a well-formed `Relation` type.
    */
  private def mkRelation(ts0: List[Type], loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkPredicate(Ast.Denotation.Relational, ts0, loc)
  }

  /**
    * Creates a well-formed `Lattice` type.
    */
  private def mkLattice(ts0: List[Type], loc: SourceLocation): Validation[Type, ResolutionError] = {
    mkPredicate(Ast.Denotation.Latticenal, ts0, loc)
  }

  /**
   * Creates the synthetic type classes.
   */
  private def mkSynthClasses()(implicit flix: Flix): List[(Symbol.ClassSym, ResolvedAst.Class)] = {
    val classes = List(mkShowClass())
    // TODO: Eq, Ord, Hash

    classes.map(clazz => clazz.sym -> clazz)
  }

  /**
   * Creates the synthetic `Show` type class.
   * {{{
   *   trait Show[a] {
   *     def show(x: a): Str
   *   }
   * }}}
   *
   */
  private def mkShowClass()(implicit flix: Flix): ResolvedAst.Class = {
    val classSym = Symbol.mkClassSym(Name.RootNS, mkSynthIdent("Show"))

    val tparamType = Type.freshVar(Kind.Star)
    val tparam = ResolvedAst.TypeParam(mkSynthIdent("a"), tparamType, SourceLocation.Generated)
    val showSig = Sig(doc = synthDoc,
      ann = Nil,
      mod = Ast.Modifiers(List(Ast.Modifier.Synthetic)),
      sym = Symbol.mkSigSym(classSym, mkSynthIdent("show")),
      tparams = List(tparam),
      fparams = List(ResolvedAst.FormalParam(Symbol.freshVarSym(), Ast.Modifiers.Empty, tparamType, SourceLocation.Generated)),
      sc = Scheme.generalize(Type.mkPureArrow(tparamType, Type.Str)),
      eff = Type.Pure,
      loc = SourceLocation.Generated)

    ResolvedAst.Class(doc = synthDoc,
      mod = Ast.Modifiers(List(Ast.Modifier.Public, Ast.Modifier.Synthetic)),
      sym = classSym,
      tparam = tparam,
      signatures = List(showSig),
      loc = SourceLocation.Generated)
  }

  /**
   * Creates a synthetic Ident.
   */
  private def mkSynthIdent(name: String): Name.Ident = Name.Ident(SourcePosition.Unknown, name, SourcePosition.Unknown)

  /**
   * Synthetic documentation.
   */
  private val synthDoc: Ast.Doc = Ast.Doc(List(), SourceLocation.Generated)
}
