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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation

import scala.collection.mutable

/**
  * The Resolver phase performs name resolution on the program.
  */
object Resolver extends Phase[NamedAst.Root, ResolvedAst.Program] {

  /**
    * The maximum depth to which type aliases are unfolded.
    */
  val RecursionLimit: Int = 25

  /**
    * Performs name resolution on the given program `prog0`.
    */
  def run(prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Program, ResolutionError] = flix.phase("Resolver") {

    val definitionsVal = prog0.defs.flatMap {
      case (ns0, defs) => defs.map {
        case (_, defn) => resolve(defn, ns0, prog0) map {
          case d => d.sym -> d
        }
      }
    }


    val namedVal = prog0.named.map {
      case (sym, exp0) => Expressions.resolve(exp0, Map.empty, Name.RootNS, prog0).map {
        case exp =>
          // Introduce a synthetic definition for the expression.
          val doc = Ast.Doc(Nil, SourceLocation.Unknown)
          val ann = Ast.Annotations.Empty
          val mod = Ast.Modifiers(Ast.Modifier.Public :: Ast.Modifier.EntryPoint :: Nil)
          val tparams = Nil
          val fparam = ResolvedAst.FormalParam(Symbol.freshVarSym("_unit"), Ast.Modifiers.Empty, Type.Unit, SourceLocation.Unknown)
          val fparams = List(fparam)
          val sc = Scheme(Nil, Type.freshTypeVar())
          val eff = Type.freshTypeVar()
          val loc = SourceLocation.Unknown
          val defn = ResolvedAst.Def(doc, ann, mod, sym, tparams, fparams, exp, sc, eff, loc)
          sym -> defn
      }
    }

    val enumsVal = prog0.enums.flatMap {
      case (ns0, enums) => enums.map {
        case (_, enum) => resolve(enum, ns0, prog0) map {
          case d => d.sym -> d
        }
      }
    }

    val relationsVal = prog0.relations.flatMap {
      case (ns0, relations) => relations.map {
        case (_, relation) => resolveRelation(relation, ns0, prog0) map {
          case t => t.sym -> t
        }
      }
    }

    val latticesVal = prog0.lattices.flatMap {
      case (ns0, lattices) => lattices.map {
        case (_, lattice) => resolveLattice(lattice, ns0, prog0) map {
          case t => t.sym -> t
        }
      }
    }

    val latticeComponentsVal = prog0.latticeComponents.map {
      case (tpe0, lattice0) =>
        for {
          tpe <- lookupType(tpe0, lattice0.ns, prog0)
          lattice <- resolve(lattice0, lattice0.ns, prog0)
        } yield (tpe, lattice)
    }

    val propertiesVal = traverse(prog0.properties) {
      case (ns0, properties) => Properties.resolve(properties, ns0, prog0)
    }

    for {
      definitions <- sequence(definitionsVal)
      named <- sequence(namedVal)
      enums <- sequence(enumsVal)
      relations <- sequence(relationsVal)
      lattices <- sequence(latticesVal)
      latticeComponents <- sequence(latticeComponentsVal)
      properties <- propertiesVal
    } yield ResolvedAst.Program(
      definitions.toMap ++ named.toMap, enums.toMap,
      relations.toMap, lattices.toMap, latticeComponents.toMap, properties.flatten, prog0.reachable, prog0.sources
    )
  }

  object Constraints {

    /**
      * Performs name resolution on the given `constraints` in the given namespace `ns0`.
      */
    def resolve(constraints: List[NamedAst.Constraint], tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.Constraint], ResolutionError] = {
      traverse(constraints)(c => resolve(c, tenv0, ns0, prog0))
    }

    /**
      * Performs name resolution on the given constraint `c0` in the given namespace `ns0`.
      */
    def resolve(c0: NamedAst.Constraint, tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Constraint, ResolutionError] = {
      for {
        ps <- traverse(c0.cparams)(p => Params.resolve(p, ns0, prog0))
        h <- Predicates.Head.resolve(c0.head, tenv0, ns0, prog0)
        bs <- traverse(c0.body)(b => Predicates.Body.resolve(b, tenv0, ns0, prog0))
      } yield ResolvedAst.Constraint(ps, h, bs, c0.loc)
    }

  }

  /**
    * Performs name resolution on the given definition `d0` in the given namespace `ns0`.
    */
  def resolve(d0: NamedAst.Def, ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Def, ResolutionError] = d0 match {
    case NamedAst.Def(doc, ann, mod, sym, tparams0, fparams0, exp0, sc0, eff0, loc) =>
      val fparam = fparams0.head

      for {
        fparamType <- lookupType(fparam.tpe, ns0, prog0)
        fparams <- resolveFormalParams(fparams0, ns0, prog0)
        tparams <- resolveTypeParams(tparams0, ns0, prog0)
        exp <- Expressions.resolve(exp0, Map(fparam.sym -> fparamType), ns0, prog0)
        scheme <- resolveScheme(sc0, ns0, prog0)
        eff <- lookupType(eff0, ns0, prog0)
      } yield ResolvedAst.Def(doc, ann, mod, sym, tparams, fparams, exp, scheme, eff, loc)
  }

  /**
    * Performs name resolution on the given enum `e0` in the given namespace `ns0`.
    */
  def resolve(e0: NamedAst.Enum, ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Enum, ResolutionError] = {
    val tparamsVal = traverse(e0.tparams)(p => Params.resolve(p, ns0, prog0))
    val casesVal = traverse(e0.cases) {
      case (name, NamedAst.Case(enum, tag, tpe)) =>
        for {
          t <- lookupType(tpe, ns0, prog0)
        } yield name -> ResolvedAst.Case(enum, tag, t)
    }
    for {
      tparams <- tparamsVal
      cases <- casesVal
      tpe <- lookupType(e0.tpe, ns0, prog0)
    } yield ResolvedAst.Enum(e0.doc, e0.mod, e0.sym, tparams, cases.toMap, tpe, e0.loc)
  }

  /**
    * Performs name resolution on the given lattice `l0` in the given namespace `ns0`.
    */
  def resolve(l0: NamedAst.LatticeComponents, ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.LatticeComponents, ResolutionError] = {
    val tenv0 = Map.empty[Symbol.VarSym, Type]
    for {
      tpe <- lookupType(l0.tpe, ns0, prog0)
      bot <- Expressions.resolve(l0.bot, tenv0, ns0, prog0)
      top <- Expressions.resolve(l0.top, tenv0, ns0, prog0)
      equ <- Expressions.resolve(l0.equ, tenv0, ns0, prog0)
      leq <- Expressions.resolve(l0.leq, tenv0, ns0, prog0)
      lub <- Expressions.resolve(l0.lub, tenv0, ns0, prog0)
      glb <- Expressions.resolve(l0.glb, tenv0, ns0, prog0)
    } yield ResolvedAst.LatticeComponents(tpe, bot, top, equ, leq, lub, glb, ns0, l0.loc)
  }

  /**
    * Performs name resolution on the given relation `r0` in the given namespace `ns0`.
    */
  def resolveRelation(r0: NamedAst.Relation, ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Relation, ResolutionError] = r0 match {
    case NamedAst.Relation(doc, mod, sym, tparams0, attr, loc) =>
      for {
        tparams <- resolveTypeParams(tparams0, ns0, prog0)
        attributes <- traverse(attr)(a => resolve(a, ns0, prog0))
      } yield {
        val ts = attributes.map(_.tpe)
        val base = Type.Cst(TypeConstructor.Relation(sym)): Type
        val args: Type = ts match {
          case Nil => Type.Unit
          case x :: Nil => x
          case l =>
            val init = Type.Cst(TypeConstructor.Tuple(l.length)): Type
            ts.foldLeft(init) {
              case (acc, t) => Type.Apply(acc, t)
            }
        }

        val tpe = Type.Apply(base, args)
        val quantifiers = tparams.map(_.tpe)
        val scheme = Scheme(quantifiers, tpe)

        ResolvedAst.Relation(doc, mod, sym, tparams, attributes, scheme, loc)
      }
  }

  /**
    * Performs name resolution on the given table `t0` in the given namespace `ns0`.
    */
  def resolveLattice(l0: NamedAst.Lattice, ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Lattice, ResolutionError] = l0 match {
    case NamedAst.Lattice(doc, mod, sym, tparams0, attr, loc) =>
      for {
        tparams <- resolveTypeParams(tparams0, ns0, prog0)
        attributes <- traverse(attr)(a => resolve(a, ns0, prog0))
      } yield {
        val ts = attributes.map(_.tpe)
        val base = Type.Cst(TypeConstructor.Lattice(sym)): Type
        val args: Type = ts match {
          case Nil => Type.Unit
          case x :: Nil => x
          case l =>
            val init = Type.Cst(TypeConstructor.Tuple(l.length)): Type
            ts.foldLeft(init) {
              case (acc, t) => Type.Apply(acc, t)
            }
        }
        val tpe = Type.Apply(base, args)

        val quantifiers = tparams.map(_.tpe)
        val scheme = Scheme(quantifiers, tpe)

        ResolvedAst.Lattice(doc, mod, sym, tparams, attributes, scheme, loc)
      }
  }

  /**
    * Performs name resolution on the given attribute `a0` in the given namespace `ns0`.
    */
  private def resolve(a0: NamedAst.Attribute, ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Attribute, ResolutionError] = {
    for {
      tpe <- lookupType(a0.tpe, ns0, prog0)
    } yield ResolvedAst.Attribute(a0.ident, tpe, a0.loc)
  }

  object Expressions {

    /**
      * Performs name resolution on the given expression `exp0` in the namespace `ns0`.
      */
    def resolve(exp0: NamedAst.Expression, tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Expression, ResolutionError] = {
      /**
        * Local visitor.
        */
      def visit(e0: NamedAst.Expression, tenv0: Map[Symbol.VarSym, Type]): Validation[ResolvedAst.Expression, ResolutionError] = e0 match {

        case NamedAst.Expression.Wild(tvar, loc) =>
          ResolvedAst.Expression.Wild(tvar, loc).toSuccess

        case NamedAst.Expression.Var(sym, loc) => tenv0.get(sym) match {
          case None => ResolvedAst.Expression.Var(sym, sym.tvar, loc).toSuccess
          case Some(tpe) =>
            // We always open schema types.
            ResolvedAst.Expression.Var(sym, Typer.openSchemaType(tpe), loc).toSuccess
        }

        case NamedAst.Expression.Def(qname, tvar, loc) =>
          lookupQName(qname, ns0, prog0) map {
            case LookupResult.Def(sym) => ResolvedAst.Expression.Def(sym, tvar, loc)
          }

        case NamedAst.Expression.Hole(nameOpt, tpe, evar, loc) =>
          val sym = nameOpt match {
            case None => Symbol.freshHoleSym(loc)
            case Some(name) => Symbol.mkHoleSym(ns0, name)
          }
          ResolvedAst.Expression.Hole(sym, tpe, evar, loc).toSuccess

        case NamedAst.Expression.Unit(loc) => ResolvedAst.Expression.Unit(loc).toSuccess

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

        case NamedAst.Expression.Apply(exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Apply(e1, e2, tvar, evar, loc)

        case NamedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
          for {
            paramType <- lookupType(fparam.tpe, ns0, prog0)
            e <- visit(exp, tenv0 + (fparam.sym -> paramType))
            p <- Params.resolve(fparam, ns0, prog0)
          } yield ResolvedAst.Expression.Lambda(p, e, tvar, loc)

        case NamedAst.Expression.Unary(op, exp, tvar, evar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Unary(op, e, tvar, evar, loc)

        case NamedAst.Expression.Binary(op, exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Binary(op, e1, e2, tvar, evar, loc)

        case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
            e3 <- visit(exp3, tenv0)
          } yield ResolvedAst.Expression.IfThenElse(e1, e2, e3, tvar, evar, loc)

        case NamedAst.Expression.Stm(exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Stm(e1, e2, tvar, evar, loc)

        case NamedAst.Expression.Let(sym, exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Let(sym, e1, e2, tvar, evar, loc)

        case NamedAst.Expression.LetRec(sym, exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.LetRec(sym, e1, e2, tvar, evar, loc)

        case NamedAst.Expression.Match(exp, rules, tvar, evar, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.MatchRule(pat, guard, body) =>
              for {
                p <- Patterns.resolve(pat, ns0, prog0)
                g <- visit(guard, tenv0)
                b <- visit(body, tenv0)
              } yield ResolvedAst.MatchRule(p, g, b)
          }

          for {
            e <- visit(exp, tenv0)
            rs <- rulesVal
          } yield ResolvedAst.Expression.Match(e, rs, tvar, evar, loc)

        case NamedAst.Expression.Tag(enum, tag, expOpt, tvar, evar, loc) => expOpt match {
          case None =>
            // Case 1: The tag has does not have an expression.
            // Either it is implicitly Unit or the tag is used as a function.

            // Lookup the enum to determine the type of the tag.
            lookupEnumByTag(enum, tag, ns0, prog0) map {
              case decl =>
                // Retrieve the relevant case.
                val caze = decl.cases(tag.name)

                // Check if the tag value has Unit type.
                if (isUnitType(caze.tpe)) {
                  // Case 1.1: The tag value has Unit type. Construct the Unit expression.
                  val e = ResolvedAst.Expression.Unit(loc)
                  ResolvedAst.Expression.Tag(decl.sym, tag.name, e, tvar, evar, loc)
                } else {
                  // Case 1.2: The tag has a non-Unit type. Hence the tag is used as a function.
                  // If the tag is `Some` we construct the lambda: x -> Some(x).

                  // Construct a fresh symbol for the formal parameter.
                  val freshVar = Symbol.freshVarSym("x")

                  // Construct the formal parameter for the fresh symbol.
                  val freshParam = ResolvedAst.FormalParam(freshVar, Ast.Modifiers.Empty, Type.freshTypeVar(), loc)

                  // Construct a variable expression for the fresh symbol.
                  val varExp = ResolvedAst.Expression.Var(freshVar, freshVar.tvar, loc)

                  // Construct the tag expression on the fresh symbol expression.
                  val tagExp = ResolvedAst.Expression.Tag(decl.sym, caze.tag.name, varExp, Type.freshTypeVar(), evar, loc)

                  // Assemble the lambda expressions.
                  ResolvedAst.Expression.Lambda(freshParam, tagExp, Type.freshTypeVar(), loc)
                }
            }
          case Some(exp) =>
            // Case 2: The tag has an expression. Perform resolution on it.
            for {
              d <- lookupEnumByTag(enum, tag, ns0, prog0)
              e <- visit(exp, tenv0)
            } yield ResolvedAst.Expression.Tag(d.sym, tag.name, e, tvar, evar, loc)
        }

        case NamedAst.Expression.Tuple(elms, tvar, evar, loc) =>
          for {
            es <- traverse(elms)(e => visit(e, tenv0))
          } yield ResolvedAst.Expression.Tuple(es, tvar, evar, loc)

        case NamedAst.Expression.RecordEmpty(tvar, loc) =>
          ResolvedAst.Expression.RecordEmpty(tvar, loc).toSuccess

        case NamedAst.Expression.RecordSelect(base, label, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
          } yield ResolvedAst.Expression.RecordSelect(b, label.name, tvar, evar, loc)

        case NamedAst.Expression.RecordExtend(label, value, rest, tvar, evar, loc) =>
          for {
            v <- visit(value, tenv0)
            r <- visit(rest, tenv0)
          } yield ResolvedAst.Expression.RecordExtend(label.name, v, r, tvar, evar, loc)

        case NamedAst.Expression.RecordRestrict(label, rest, tvar, evar, loc) =>
          for {
            r <- visit(rest, tenv0)
          } yield ResolvedAst.Expression.RecordRestrict(label.name, r, tvar, evar, loc)

        case NamedAst.Expression.ArrayLit(elms, tvar, evar, loc) =>
          for {
            es <- traverse(elms)(e => visit(e, tenv0))
          } yield ResolvedAst.Expression.ArrayLit(es, tvar, evar, loc)

        case NamedAst.Expression.ArrayNew(elm, len, tvar, evar, loc) =>
          for {
            e <- visit(elm, tenv0)
            ln <- visit(len, tenv0)
          } yield ResolvedAst.Expression.ArrayNew(e, ln, tvar, evar, loc)

        case NamedAst.Expression.ArrayLoad(base, index, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
            i <- visit(index, tenv0)
          } yield ResolvedAst.Expression.ArrayLoad(b, i, tvar, evar, loc)

        case NamedAst.Expression.ArrayStore(base, index, elm, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
            i <- visit(index, tenv0)
            e <- visit(elm, tenv0)
          } yield ResolvedAst.Expression.ArrayStore(b, i, e, tvar, evar, loc)

        case NamedAst.Expression.ArrayLength(base, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
          } yield ResolvedAst.Expression.ArrayLength(b, tvar, evar, loc)

        case NamedAst.Expression.ArraySlice(base, startIndex, endIndex, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
            i1 <- visit(startIndex, tenv0)
            i2 <- visit(endIndex, tenv0)
          } yield ResolvedAst.Expression.ArraySlice(b, i1, i2, tvar, evar, loc)

        case NamedAst.Expression.VectorLit(elms, tvar, evar, loc) =>
          for {
            es <- traverse(elms)(e => visit(e, tenv0))
          } yield ResolvedAst.Expression.VectorLit(es, tvar, evar, loc)

        case NamedAst.Expression.VectorNew(elm, len, tvar, evar, loc) =>
          for {
            e <- visit(elm, tenv0)
          } yield ResolvedAst.Expression.VectorNew(e, len, tvar, evar, loc)

        case NamedAst.Expression.VectorLoad(base, index, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
          } yield ResolvedAst.Expression.VectorLoad(b, index, tvar, evar, loc)

        case NamedAst.Expression.VectorStore(base, index, elm, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
            e <- visit(elm, tenv0)
          } yield ResolvedAst.Expression.VectorStore(b, index, e, tvar, evar, loc)

        case NamedAst.Expression.VectorLength(base, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
          } yield ResolvedAst.Expression.VectorLength(b, tvar, evar, loc)

        case NamedAst.Expression.VectorSlice(base, startIndex, optEndIndex, tvar, evar, loc) =>
          for {
            b <- visit(base, tenv0)
          } yield ResolvedAst.Expression.VectorSlice(b, startIndex, optEndIndex, tvar, evar, loc)

        case NamedAst.Expression.Ref(exp, tvar, evar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Ref(e, tvar, evar, loc)

        case NamedAst.Expression.Deref(exp, tvar, evar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Deref(e, tvar, evar, loc)

        case NamedAst.Expression.Assign(exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.Assign(e1, e2, tvar, evar, loc)

        case NamedAst.Expression.Existential(fparam, exp, loc) =>
          for {
            fp <- Params.resolve(fparam, ns0, prog0)
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Existential(fp, e, loc)

        case NamedAst.Expression.Universal(fparam, exp, loc) =>
          for {
            fp <- Params.resolve(fparam, ns0, prog0)
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.Universal(fp, e, loc)

        case NamedAst.Expression.Ascribe(exp, expectedType, expectedEff, tvar, evar, loc) =>
          val expectedTypVal = expectedType match {
            case None => (None: Option[Type]).toSuccess
            case Some(t) => mapN(lookupType(t, ns0, prog0))(x => Some(x))
          }
          val expectedEffVal = expectedEff match {
            case None => (None: Option[Type]).toSuccess
            case Some(f) => mapN(lookupType(f, ns0, prog0))(x => Some(x))
          }

          for {
            e <- visit(exp, tenv0)
            t <- expectedTypVal
            f <- expectedEffVal
          } yield ResolvedAst.Expression.Ascribe(e, t, f, tvar, evar, loc)

        case NamedAst.Expression.Cast(exp, declaredType, declaredEff, tvar, evar, loc) =>
          val declaredTypVal = declaredType match {
            case None => (None: Option[Type]).toSuccess
            case Some(t) => mapN(lookupType(t, ns0, prog0))(x => Some(x))
          }
          val declaredEffVal = declaredEff match {
            case None => (None: Option[Type]).toSuccess
            case Some(f) => mapN(lookupType(f, ns0, prog0))(x => Some(x))
          }

          for {
            e <- visit(exp, tenv0)
            t <- declaredTypVal
            f <- declaredEffVal
          } yield ResolvedAst.Expression.Cast(e, t, f, tvar, evar, loc)

        case NamedAst.Expression.TryCatch(exp, rules, tpe, evar, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.CatchRule(sym, clazz, body) =>
              val exceptionType = Type.Cst(TypeConstructor.Native(clazz))
              visit(body, tenv0 + (sym -> exceptionType)) map {
                case b => ResolvedAst.CatchRule(sym, clazz, b)
              }
          }

          for {
            e <- visit(exp, tenv0)
            rs <- rulesVal
          } yield ResolvedAst.Expression.TryCatch(e, rs, tpe, evar, loc)

        case NamedAst.Expression.InvokeConstructor(className, args, sig, tvar, evar, loc) =>
          val argsVal = traverse(args)(visit(_, tenv0))
          val sigVal = traverse(sig)(lookupType(_, ns0, prog0))
          flatMapN(sigVal, argsVal) {
            case (ts, as) =>
              mapN(lookupJvmConstructor(className, ts, loc)) {
                case constructor => ResolvedAst.Expression.InvokeConstructor(constructor, as, tvar, evar, loc)
              }
          }

        case NamedAst.Expression.InvokeMethod(className, methodName, exp, args, sig, tvar, evar, loc) =>
          val expVal = visit(exp, tenv0)
          val argsVal = traverse(args)(visit(_, tenv0))
          val sigVal = traverse(sig)(lookupType(_, ns0, prog0))
          flatMapN(sigVal, expVal, argsVal) {
            case (ts, e, as) =>
              mapN(lookupJvmMethod(className, methodName, ts, static = false, loc)) {
                case method => ResolvedAst.Expression.InvokeMethod(method, e, as, tvar, evar, loc)
              }
          }

        case NamedAst.Expression.InvokeStaticMethod(className, methodName, args, sig, tvar, evar, loc) =>
          val argsVal = traverse(args)(visit(_, tenv0))
          val sigVal = traverse(sig)(lookupType(_, ns0, prog0))
          flatMapN(sigVal, argsVal) {
            case (ts, as) =>
              mapN(lookupJvmMethod(className, methodName, ts, static = true, loc)) {
                case method => ResolvedAst.Expression.InvokeStaticMethod(method, as, tvar, evar, loc)
              }
          }

        case NamedAst.Expression.GetField(className, fieldName, exp, tvar, evar, loc) =>
          mapN(lookupJvmField(className, fieldName, static = false, loc), visit(exp, tenv0)) {
            case (field, e) => ResolvedAst.Expression.GetField(field, e, tvar, evar, loc)
          }

        case NamedAst.Expression.PutField(className, fieldName, exp1, exp2, tvar, evar, loc) =>
          mapN(lookupJvmField(className, fieldName, static = false, loc), visit(exp1, tenv0), visit(exp2, tenv0)) {
            case (field, e1, e2) => ResolvedAst.Expression.PutField(field, e1, e2, tvar, evar, loc)
          }

        case NamedAst.Expression.GetStaticField(className, fieldName, tvar, evar, loc) =>
          mapN(lookupJvmField(className, fieldName, static = true, loc)) {
            case field => ResolvedAst.Expression.GetStaticField(field, tvar, evar, loc)
          }

        case NamedAst.Expression.PutStaticField(className, fieldName, exp, tvar, evar, loc) =>
          mapN(lookupJvmField(className, fieldName, static = true, loc), visit(exp, tenv0)) {
            case (field, e) => ResolvedAst.Expression.PutStaticField(field, e, tvar, evar, loc)
          }

        case NamedAst.Expression.NewChannel(exp, tpe, evar, loc) =>
          for {
            t <- lookupType(tpe, ns0, prog0)
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.NewChannel(e, t, evar, loc)

        case NamedAst.Expression.GetChannel(exp, tvar, evar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.GetChannel(e, tvar, evar, loc)

        case NamedAst.Expression.PutChannel(exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.PutChannel(e1, e2, tvar, evar, loc)

        case NamedAst.Expression.SelectChannel(rules, default, tvar, evar, loc) =>
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
          } yield ResolvedAst.Expression.SelectChannel(rs, d, tvar, evar, loc)

        case NamedAst.Expression.ProcessSpawn(exp, tvar, evar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.ProcessSpawn(e, tvar, evar, loc)

        case NamedAst.Expression.ProcessPanic(msg, tvar, evar, loc) =>
          ResolvedAst.Expression.ProcessPanic(msg, tvar, evar, loc).toSuccess

        case NamedAst.Expression.FixpointConstraintSet(cs0, tvar, loc) =>
          for {
            cs <- traverse(cs0)(Constraints.resolve(_, tenv0, ns0, prog0))
          } yield ResolvedAst.Expression.FixpointConstraintSet(cs, tvar, loc)

        case NamedAst.Expression.FixpointCompose(exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.FixpointCompose(e1, e2, tvar, evar, loc)

        case NamedAst.Expression.FixpointSolve(exp, tvar, evar, loc) =>
          for {
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.FixpointSolve(e, tvar, evar, loc)

        case NamedAst.Expression.FixpointProject(qname, exp, tvar, evar, loc) =>
          for {
            sym <- lookupPredicateSymbol(qname, ns0, prog0)
            e <- visit(exp, tenv0)
          } yield ResolvedAst.Expression.FixpointProject(sym, e, tvar, evar, loc)

        case NamedAst.Expression.FixpointEntails(exp1, exp2, tvar, evar, loc) =>
          for {
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
          } yield ResolvedAst.Expression.FixpointEntails(e1, e2, tvar, evar, loc)

        case NamedAst.Expression.FixpointFold(qname, exp1, exp2, exp3, tvar, evar, loc) =>
          for {
            sym <- lookupPredicateSymbol(qname, ns0, prog0)
            e1 <- visit(exp1, tenv0)
            e2 <- visit(exp2, tenv0)
            e3 <- visit(exp3, tenv0)
          } yield ResolvedAst.Expression.FixpointFold(sym, e1, e2, e3, tvar, evar, loc)
      }

      visit(exp0, Map.empty)
    }

  }

  object Patterns {

    /**
      * Performs name resolution on the given pattern `pat0` in the namespace `ns0`.
      */
    def resolve(pat0: NamedAst.Pattern, ns0: Name.NName, prog0: NamedAst.Root): Validation[ResolvedAst.Pattern, ResolutionError] = {

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
            d <- lookupEnumByTag(enum, tag, ns0, prog0)
            p <- visit(pat)
          } yield ResolvedAst.Pattern.Tag(d.sym, tag.name, p, tvar, loc)

        case NamedAst.Pattern.Tuple(elms, tvar, loc) =>
          for {
            es <- traverse(elms)(visit)
          } yield ResolvedAst.Pattern.Tuple(es, tvar, loc)

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
      def resolve(h0: NamedAst.Predicate.Head, tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Predicate.Head, ResolutionError] = h0 match {
        case NamedAst.Predicate.Head.Atom(qname, den, terms, tvar, loc) =>
          for {
            sym <- lookupPredicateSymbol(qname, ns0, prog0)
            ts <- traverse(terms)(t => Expressions.resolve(t, tenv0, ns0, prog0))
          } yield ResolvedAst.Predicate.Head.Atom(sym, den, ts, tvar, loc)

        case NamedAst.Predicate.Head.Union(exp, tvar, loc) =>
          for {
            e <- Expressions.resolve(exp, tenv0, ns0, prog0)
          } yield ResolvedAst.Predicate.Head.Union(e, tvar, loc)
      }
    }

    object Body {
      /**
        * Performs name resolution on the given body predicate `b0` in the given namespace `ns0`.
        */
      def resolve(b0: NamedAst.Predicate.Body, tenv0: Map[Symbol.VarSym, Type], ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Predicate.Body, ResolutionError] = b0 match {
        case NamedAst.Predicate.Body.Atom(qname, den, polarity, terms, tvar, loc) =>
          for {
            sym <- lookupPredicateSymbol(qname, ns0, prog0)
            ts <- traverse(terms)(t => Patterns.resolve(t, ns0, prog0))
          } yield ResolvedAst.Predicate.Body.Atom(sym, den, polarity, ts, tvar, loc)

        case NamedAst.Predicate.Body.Guard(exp, loc) =>
          for {
            e <- Expressions.resolve(exp, tenv0, ns0, prog0)
          } yield ResolvedAst.Predicate.Body.Guard(e, loc)
      }
    }

  }

  object Properties {

    /**
      * Performs name resolution on each of the given `properties` in the given namespace `ns0`.
      */
    def resolve(properties: List[NamedAst.Property], ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.Property], ResolutionError] = {
      traverse(properties)(p => resolve(p, ns0, prog0))
    }

    /**
      * Performs name resolution on the given property `p0` in the given namespace `ns0`.
      */
    def resolve(p0: NamedAst.Property, ns0: Name.NName, prog0: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Property, ResolutionError] = {
      for {
        e <- Expressions.resolve(p0.exp, Map.empty, ns0, prog0)
      } yield ResolvedAst.Property(p0.law, p0.defn, e, p0.loc)
    }

  }

  object Params {

    /**
      * Performs name resolution on the given constraint parameter `cparam0` in the given namespace `ns0`.
      */
    def resolve(cparam0: NamedAst.ConstraintParam, ns0: Name.NName, prog0: NamedAst.Root): Validation[ResolvedAst.ConstraintParam, ResolutionError] = cparam0 match {
      case NamedAst.ConstraintParam.HeadParam(sym, tpe, loc) => ResolvedAst.ConstraintParam.HeadParam(sym, tpe, loc).toSuccess
      case NamedAst.ConstraintParam.RuleParam(sym, tpe, loc) => ResolvedAst.ConstraintParam.RuleParam(sym, tpe, loc).toSuccess
    }

    /**
      * Performs name resolution on the given formal parameter `fparam0` in the given namespace `ns0`.
      */
    def resolve(fparam0: NamedAst.FormalParam, ns0: Name.NName, prog0: NamedAst.Root): Validation[ResolvedAst.FormalParam, ResolutionError] = {
      for {
        t <- lookupType(fparam0.tpe, ns0, prog0)
      } yield ResolvedAst.FormalParam(fparam0.sym, fparam0.mod, t, fparam0.loc)
    }

    /**
      * Performs name resolution on the given type parameter `tparam0` in the given namespace `ns0`.
      */
    def resolve(tparam0: NamedAst.TypeParam, ns0: Name.NName, prog0: NamedAst.Root): Validation[ResolvedAst.TypeParam, ResolutionError] = {
      ResolvedAst.TypeParam(tparam0.name, tparam0.tpe, tparam0.loc).toSuccess
    }

  }

  /**
    * Performs name resolution on the given formal parameters `fparams0`.
    */
  def resolveFormalParams(fparams0: List[NamedAst.FormalParam], ns0: Name.NName, prog0: NamedAst.Root): Validation[List[ResolvedAst.FormalParam], ResolutionError] = {
    traverse(fparams0)(fparam => Params.resolve(fparam, ns0, prog0))
  }

  /**
    * Performs name resolution on the given type parameters `tparams0`.
    */
  def resolveTypeParams(tparams0: List[NamedAst.TypeParam], ns0: Name.NName, prog0: NamedAst.Root): Validation[List[ResolvedAst.TypeParam], ResolutionError] =
    traverse(tparams0)(tparam => Params.resolve(tparam, ns0, prog0))

  /**
    * Performs name resolution on the given scheme `sc0`.
    */
  def resolveScheme(sc0: NamedAst.Scheme, ns0: Name.NName, prog0: NamedAst.Root): Validation[Scheme, ResolutionError] = {
    for {
      base <- lookupType(sc0.base, ns0, prog0)
    } yield Scheme(sc0.quantifiers, base)
  }

  /**
    * The result of a qualified name lookup.
    */
  sealed trait LookupResult

  object LookupResult {

    case class Def(sym: Symbol.DefnSym) extends LookupResult

  }

  /**
    * Finds the definition with the qualified name `qname` in the namespace `ns0`.
    */
  def lookupQName(qname: Name.QName, ns0: Name.NName, prog0: NamedAst.Root): Validation[LookupResult, ResolutionError] = {
    val defOpt = tryLookupDef(qname, ns0, prog0)

    defOpt match {
      case None => ResolutionError.UndefinedName(qname, ns0, qname.loc).toFailure
      case Some(d) => getDefIfAccessible(d, ns0, qname.loc)
    }
  }

  /**
    * Tries to a def with the qualified name `qname` in the namespace `ns0`.
    */
  def tryLookupDef(qname: Name.QName, ns0: Name.NName, prog0: NamedAst.Root): Option[NamedAst.Def] = {
    // Check whether the name is fully-qualified.
    if (qname.isUnqualified) {
      // Case 1: Unqualified name. Lookup in the current namespace.
      val defnOpt = prog0.defs.getOrElse(ns0, Map.empty).get(qname.ident.name)

      defnOpt match {
        case Some(defn) =>
          // Case 1.2: Found in the current namespace.
          Some(defn)
        case None =>
          // Case 1.1: Try the global namespace.
          prog0.defs.getOrElse(Name.RootNS, Map.empty).get(qname.ident.name)
      }
    } else {
      // Case 2: Qualified. Lookup in the given namespace.
      prog0.defs.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Finds the enum definition matching the given qualified name and tag.
    */
  def lookupEnumByTag(qname: Option[Name.QName], tag: Name.Ident, ns: Name.NName, prog0: NamedAst.Root): Validation[NamedAst.Enum, ResolutionError] = {
    /*
     * Lookup the tag name in all enums across all namespaces.
     */
    val globalMatches = mutable.Set.empty[NamedAst.Enum]
    for ((_, decls) <- prog0.enums) {
      for ((enumName, decl) <- decls) {
        for ((tagName, caze) <- decl.cases) {
          if (tag.name == tagName) {
            globalMatches += decl
          }
        }
      }
    }

    // Case 1: Exact match found. Simply return it.
    if (globalMatches.size == 1) {
      return getEnumIfAccessible(globalMatches.head, ns, tag.loc)
    }

    // Case 2: No or multiple matches found.
    // Lookup the tag in either the fully qualified namespace or the current namespace.
    val namespace = if (qname.exists(_.isQualified)) qname.get.namespace else ns

    /*
     * Lookup the tag name in all enums in the current namespace.
     */
    val namespaceMatches = mutable.Set.empty[NamedAst.Enum]
    for ((enumName, decl) <- prog0.enums.getOrElse(namespace, Map.empty[String, NamedAst.Enum])) {
      for ((tagName, caze) <- decl.cases) {
        if (tag.name == tagName) {
          namespaceMatches += decl
        }
      }
    }

    // Case 2.1: Exact match found in namespace. Simply return it.
    if (namespaceMatches.size == 1) {
      return getEnumIfAccessible(namespaceMatches.head, ns, tag.loc)
    }

    // Case 2.2: No matches found in namespace.
    if (namespaceMatches.isEmpty) {
      return ResolutionError.UndefinedTag(tag.name, ns, tag.loc).toFailure
    }

    // Case 2.3: Multiple matches found in namespace and no enum name.
    if (qname.isEmpty) {
      val locs = namespaceMatches.map(_.sym.loc).toList.sorted
      return ResolutionError.AmbiguousTag(tag.name, ns, locs, tag.loc).toFailure
    }

    // Case 2.4: Multiple matches found in namespace and an enum name is available.
    val filteredMatches = namespaceMatches.filter(_.sym.name == qname.get.ident.name)
    if (filteredMatches.size == 1) {
      return getEnumIfAccessible(filteredMatches.head, ns, tag.loc)
    }

    ResolutionError.UndefinedTag(tag.name, ns, tag.loc).toFailure
  }

  /**
    * Finds the table of the given `qname` in the namespace `ns`.
    */
  def lookupPredicateSymbol(qname: Name.QName, ns: Name.NName, prog0: NamedAst.Root): Validation[Symbol.PredSym, ResolutionError] = {
    // Lookup the relation and lattice.
    val relationOpt = lookupRelSymbol(qname, ns, prog0)
    val latticeOpt = lookupLatSymbol(qname, ns, prog0)

    (relationOpt, latticeOpt) match {
      case (None, None) => ResolutionError.UndefinedTable(qname, ns, qname.loc).toFailure
      case (Some(rel), None) => getRelationIfAccessible(rel, ns, qname.loc)
      case (None, Some(lat)) => getLatticeIfAccessible(lat, ns, qname.loc)
      case (Some(rel), Some(lat)) => ResolutionError.AmbiguousRelationOrLattice(qname, ns, List(rel.loc, lat.loc), qname.loc).toFailure
    }
  }

  /**
    * Finds the relation of the given `qname` in the namespace `ns`.
    */
  private def lookupRelSymbol(qname: Name.QName, ns: Name.NName, prog0: NamedAst.Root): Option[NamedAst.Relation] = {
    if (qname.isUnqualified) {
      // Lookup in the current namespace.
      prog0.relations.getOrElse(ns, Map.empty).get(qname.ident.name).orElse {
        // Lookup in the global namespace.
        prog0.relations.getOrElse(Name.RootNS, Map.empty).get(qname.ident.name)
      }
    } else {
      // Lookup in the qualified namespace.
      prog0.relations.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Finds the lattice of the given `qname` in the namespace `ns`.
    */
  private def lookupLatSymbol(qname: Name.QName, ns: Name.NName, prog0: NamedAst.Root): Option[NamedAst.Lattice] = {
    if (qname.isUnqualified) {
      // Lookup in the current namespace.
      prog0.lattices.getOrElse(ns, Map.empty).get(qname.ident.name).orElse {
        // Lookup in the global namespace.
        prog0.lattices.getOrElse(Name.RootNS, Map.empty).get(qname.ident.name)
      }
    } else {
      // Lookup in the qualified namespace.
      prog0.lattices.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
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
      case "Array" => Type.Cst(TypeConstructor.Array).toSuccess
      case "Channel" => Type.Cst(TypeConstructor.Channel).toSuccess
      case "Ref" => Type.Cst(TypeConstructor.Ref).toSuccess
      case "Vector" => Type.Cst(TypeConstructor.Vector).toSuccess

      // Disambiguate type.
      case typeName =>
        (lookupEnum(typeName, ns0, root), lookupRelation(typeName, ns0, root), lookupLattice(typeName, ns0, root), lookupTypeAlias(typeName, ns0, root)) match {
          // Case 1: Not Found.
          case (None, None, None, None) => ResolutionError.UndefinedType(qname, ns0, loc).toFailure

          // Case 2: Enum.
          case (Some(enum), None, None, None) => getEnumTypeIfAccessible(enum, ns0, ns0.loc)

          // Case 3: Relation.
          case (None, Some(rel), None, None) => getRelationTypeIfAccessible(rel, ns0, root, ns0.loc)

          // Case 4: Lattice.
          case (None, None, Some(lat), None) => getLatticeTypeIfAccessible(lat, ns0, root, ns0.loc)

          // Case 5: TypeAlias.
          case (None, None, None, Some(typealias)) => getTypeAliasIfAccessible(typealias, ns0, root, ns0.loc)

          // Case 6: Errors.
          case (x, y, z, w) =>
            val loc1 = x.map(_.loc)
            val loc2 = y.map(_.loc)
            val loc3 = z.map(_.loc)
            val loc4 = w.map(_.loc)
            val locs = List(loc1, loc2, loc3, loc4).flatten
            ResolutionError.AmbiguousType(typeName, ns0, locs, loc).toFailure
        }
    }
    case NamedAst.Type.Ambiguous(qname, loc) if qname.isQualified =>
      // Lookup the enum using the namespace.
      val decls = root.enums.getOrElse(qname.namespace, Map.empty)
      decls.get(qname.ident.name) match {
        case None => ResolutionError.UndefinedType(qname, ns0, loc).toFailure
        case Some(enum) => getEnumTypeIfAccessible(enum, ns0, ns0.loc)
      }
    case NamedAst.Type.Enum(sym) =>
      Type.Cst(TypeConstructor.Enum(sym, Kind.Star)).toSuccess

    case NamedAst.Type.Tuple(elms0, loc) =>
      for (
        elms <- traverse(elms0)(tpe => lookupType(tpe, ns0, root))
      ) yield Type.mkTuple(elms)

    case NamedAst.Type.RecordEmpty(loc) =>
      Type.RecordEmpty.toSuccess

    case NamedAst.Type.RecordExtend(label, value, rest, loc) =>
      for {
        v <- lookupType(value, ns0, root)
        r <- lookupType(rest, ns0, root)
      } yield Type.RecordExtend(label.name, v, r)

    case NamedAst.Type.SchemaEmpty(loc) =>
      Type.SchemaEmpty.toSuccess

    case NamedAst.Type.Schema(ts, rest, loc) =>
      // Translate the type into a schema row type.
      val init = lookupType(rest, ns0, root)

      // Fold over the predicate types and check that they are relations or lattices.
      Validation.foldRight(ts)(init) {
        case (predType, acc) =>
          // Lookup the type and check that is either a relation or lattice type.
          flatMapN(lookupType(predType, ns0, root)) {
            case t =>
              // Simplify the type. Required to get the appropriate type constructor.
              val s = simplify(t)

              // Determine the type of predicate symbol.
              s.typeConstructor match {
                case Type.Cst(TypeConstructor.Relation(sym)) => Type.SchemaExtend(sym, s, acc).toSuccess
                case Type.Cst(TypeConstructor.Lattice(sym)) => Type.SchemaExtend(sym, s, acc).toSuccess
                case nonRelationOrLatticeType => ResolutionError.NonRelationOrLattice(nonRelationOrLatticeType, loc).toFailure
              }
          }
      }

    case NamedAst.Type.Nat(len, loc) => Type.Succ(len, Type.Zero).toSuccess

    case NamedAst.Type.Native(fqn, loc) =>
      fqn match {
        case "java.math.BigInteger" => Type.BigInt.toSuccess
        case "java.lang.String" => Type.Str.toSuccess
        case _ => lookupJvmClass(fqn, loc) map {
          case clazz => Type.Cst(TypeConstructor.Native(clazz))
        }
      }

    case NamedAst.Type.Arrow(tparams0, eff0, tresult0, loc) =>
      for {
        tparams <- traverse(tparams0)(lookupType(_, ns0, root));
        tresult <- lookupType(tresult0, ns0, root)
        eff <- lookupType(eff0, ns0, root)
      } yield Type.mkArrow(tparams, eff, tresult)

    case NamedAst.Type.Apply(base0, targ0, loc) =>
      for (
        tpe1 <- lookupType(base0, ns0, root);
        tpe2 <- lookupType(targ0, ns0, root)
      ) yield simplify(Type.Apply(tpe1, tpe2))

    case NamedAst.Type.Pure(loc) =>
      Type.Pure.toSuccess

    case NamedAst.Type.Impure(loc) =>
      Type.Cst(TypeConstructor.Impure).toSuccess

    case NamedAst.Type.Not(tpe, loc) =>
      mapN(lookupType(tpe, ns0, root)) {
        case t => Type.Apply(Type.Cst(TypeConstructor.Not), t)
      }

    case NamedAst.Type.And(tpe1, tpe2, loc) =>
      mapN(lookupType(tpe1, ns0, root), lookupType(tpe2, ns0, root)) {
        case (t1, t2) => Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And), t1), t2)
      }

    case NamedAst.Type.Or(tpe1, tpe2, loc) =>
      mapN(lookupType(tpe1, ns0, root), lookupType(tpe2, ns0, root)) {
        case (t1, t2) => Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or), t1), t2)
      }

  }

  /**
    * Optionally returns the enum with the given `name` in the given namespace `ns0`.
    */
  private def lookupEnum(typeName: String, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.Enum] = {
    val enumsInNamespace = root.enums.getOrElse(ns0, Map.empty)
    enumsInNamespace.get(typeName) orElse {
      val enumsInRootNS = root.enums.getOrElse(Name.RootNS, Map.empty)
      enumsInRootNS.get(typeName)
    }
  }

  /**
    * Optionally returns the relation with the given `name` in the given namespace `ns0`.
    */
  private def lookupRelation(typeName: String, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.Relation] = {
    val relationsInNS = root.relations.getOrElse(ns0, Map.empty)
    relationsInNS.get(typeName) orElse {
      val relationsInRootNS = root.relations.getOrElse(Name.RootNS, Map.empty)
      relationsInRootNS.get(typeName)
    }
  }

  /**
    * Optionally returns the lattice with the given `name` in the given namespace `ns0`.
    */
  private def lookupLattice(name: String, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.Lattice] = {
    val latticesInNS = root.lattices.getOrElse(ns0, Map.empty)
    latticesInNS.get(name) orElse {
      val latticesInRootNS = root.lattices.getOrElse(Name.RootNS, Map.empty)
      latticesInRootNS.get(name)
    }
  }

  /**
    * Optionally returns the type alias with the given `name` in the given namespace `ns0`.
    */
  private def lookupTypeAlias(typeName: String, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.TypeAlias] = {
    val typeAliasesInNamespace = root.typealiases.getOrElse(ns0, Map.empty)
    typeAliasesInNamespace.get(typeName) orElse {
      val typeAliasesInRootNS = root.typealiases.getOrElse(Name.RootNS, Map.empty)
      typeAliasesInRootNS.get(typeName)
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
  def getDefIfAccessible(defn0: NamedAst.Def, ns0: Name.NName, loc: SourceLocation): Validation[LookupResult, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (defn0.mod.isPublic)
      return LookupResult.Def(defn0.sym).toSuccess

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = defn0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return LookupResult.Def(defn0.sym).toSuccess

    //
    // The definition is not accessible.
    //
    ResolutionError.InaccessibleDef(defn0.sym, ns0, loc).toFailure
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
    * Successfully returns the given relation `rel0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * A relation `rel0` is accessible from a namespace `ns0` if:
    *
    * (a) the relation is marked public, or
    * (b) the relation is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def getRelationIfAccessible(rel0: NamedAst.Relation, ns0: Name.NName, loc: SourceLocation): Validation[Symbol.RelSym, ResolutionError] = {
    //
    // Check if the relation is marked public.
    //
    if (rel0.mod.isPublic)
      return rel0.sym.toSuccess

    //
    // Check if the relation is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = rel0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return rel0.sym.toSuccess

    //
    // The relation is not accessible.
    //
    ResolutionError.InaccessibleRelation(rel0.sym, ns0, loc).toFailure
  }

  /**
    * Successfully returns the given lattice `lat0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * A relation `lat0` is accessible from a namespace `ns0` if:
    *
    * (a) the lattice is marked public, or
    * (b) the lattice is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def getLatticeIfAccessible(lat0: NamedAst.Lattice, ns0: Name.NName, loc: SourceLocation): Validation[Symbol.LatSym, ResolutionError] = {
    //
    // Check if the lattice is marked public.
    //
    if (lat0.mod.isPublic)
      return lat0.sym.toSuccess

    //
    // Check if the lattice is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = lat0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return lat0.sym.toSuccess

    //
    // The lattice is not accessible.
    //
    ResolutionError.InaccessibleLattice(lat0.sym, ns0, loc).toFailure
  }

  /**
    * Successfully returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  def getEnumTypeIfAccessible(enum0: NamedAst.Enum, ns0: Name.NName, loc: SourceLocation): Validation[Type, ResolutionError] =
    getEnumIfAccessible(enum0, ns0, loc) map {
      case enum => Type.Cst(TypeConstructor.Enum(enum.sym, Kind.Star))
    }

  /**
    * Successfully returns the type of the given `rel0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  def getRelationTypeIfAccessible(rel0: NamedAst.Relation, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation): Validation[Type, ResolutionError] = {
    // NB: This is a small hack because the attribute types should be resolved according to the namespace of the relation.
    val declNS = getNS(rel0.sym.namespace)
    getRelationIfAccessible(rel0, ns0, loc) flatMap {
      case sym => traverse(rel0.attr)(a => lookupType(a.tpe, declNS, root)) map {
        case attr => mkRelationOrLattice(sym, rel0.tparams, attr)
      }
    }
  }

  /**
    * Successfully returns the type of the given `lat0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  def getLatticeTypeIfAccessible(lat0: NamedAst.Lattice, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation): Validation[Type, ResolutionError] = {
    // NB: This is a small hack because the attribute types should be resolved according to the namespace of the relation.
    val declNS = getNS(lat0.sym.namespace)
    getLatticeIfAccessible(lat0, ns0, loc) flatMap {
      case sym => traverse(lat0.attr)(a => lookupType(a.tpe, declNS, root)) map {
        case attr => mkRelationOrLattice(sym, lat0.tparams, attr)
      }
    }
  }

  /**
    * Returns the type corresponding to the given predicate symbol `sym` with the given attribute types `ts`.
    */
  private def mkRelationOrLattice(predSym: Symbol.PredSym, tparams: List[NamedAst.TypeParam], ts: List[Type]): Type = predSym match {
    case sym: Symbol.RelSym =>
      val base = Type.Cst(TypeConstructor.Relation(sym)): Type
      val args: Type = ts match {
        case Nil => Type.Unit
        case x :: Nil => x
        case l =>
          val init = Type.Cst(TypeConstructor.Tuple(l.length)): Type
          ts.foldLeft(init) {
            case (acc, t) => Type.Apply(acc, t)
          }
      }
      mkTypeLambda(tparams, Type.Apply(base, args))

    case sym: Symbol.LatSym =>
      val base = Type.Cst(TypeConstructor.Lattice(sym)): Type
      val args: Type = ts match {
        case Nil => Type.Unit
        case x :: Nil => x
        case l =>
          val init = Type.Cst(TypeConstructor.Tuple(l.length)): Type
          ts.foldLeft(init) {
            case (acc, t) => Type.Apply(acc, t)
          }
      }
      mkTypeLambda(tparams, Type.Apply(base, args))
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
    * Returns a simplified (evaluated) form of the given type `tpe0`.
    *
    * Performs beta-reduction of type abstractions and applications.
    */
  private def simplify(tpe0: Type): Type = {
    def eval(t: Type, subst: Map[Type.Var, Type]): Type = t match {
      case tvar: Type.Var => subst.getOrElse(tvar, tvar)
      case Type.Cst(_) => t

      case Type.Arrow(l, eff) => Type.Arrow(l, eval(eff, subst))

      case Type.RecordEmpty => t

      case Type.RecordExtend(label, value, rest) =>
        val t1 = eval(value, subst)
        val t2 = eval(rest, subst)
        Type.RecordExtend(label, t1, t2)

      case Type.SchemaEmpty => t

      case Type.SchemaExtend(sym, tpe, rest) =>
        val t1 = eval(tpe, subst)
        val t2 = eval(rest, subst)
        Type.SchemaExtend(sym, t1, t2)

      case Type.Zero => t

      case Type.Succ(len, t) => Type.Succ(len, eval(t, subst))

      case Type.Lambda(tvar, tpe) => Type.Lambda(tvar, eval(tpe, subst))

      // TODO: Does not take variable capture into account.
      case Type.Apply(tpe1, tpe2) => (eval(tpe1, subst), eval(tpe2, subst)) match {
        case (Type.Lambda(tvar, tpe3), t2) => eval(tpe3, subst + (tvar -> t2))
        case (t1, t2) => Type.Apply(t1, t2)
      }
    }

    eval(tpe0, Map.empty)
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
    case Type.Cst(TypeConstructor.Unit) => Class.forName("java.lang.Object").toSuccess

    case Type.Cst(TypeConstructor.Bool) => classOf[Boolean].toSuccess

    case Type.Cst(TypeConstructor.Char) => classOf[Char].toSuccess

    case Type.Cst(TypeConstructor.Float32) => classOf[Float].toSuccess

    case Type.Cst(TypeConstructor.Float64) => classOf[Double].toSuccess

    case Type.Cst(TypeConstructor.Int8) => classOf[Byte].toSuccess

    case Type.Cst(TypeConstructor.Int16) => classOf[Short].toSuccess

    case Type.Cst(TypeConstructor.Int32) => classOf[Int].toSuccess

    case Type.Cst(TypeConstructor.Int64) => classOf[Long].toSuccess

    case Type.Cst(TypeConstructor.BigInt) => Class.forName("java.math.BigInteger").toSuccess

    case Type.Cst(TypeConstructor.Str) => Class.forName("java.lang.String").toSuccess

    case Type.Cst(TypeConstructor.Channel) => Class.forName("java.lang.Object").toSuccess

    case Type.Cst(TypeConstructor.Enum(_, _)) => Class.forName("java.lang.Object").toSuccess

    case Type.Cst(TypeConstructor.Ref) => Class.forName("java.lang.Object").toSuccess

    case Type.Cst(TypeConstructor.Tuple(_)) => Class.forName("java.lang.Object").toSuccess

    case Type.Cst(TypeConstructor.Array) =>
      tpe.typeArguments match {
        case elmTyp :: Nil =>
          mapN(getJVMType(elmTyp, loc)) {
            case elmClass =>
              // See: https://stackoverflow.com/questions/1679421/how-to-get-the-array-class-for-a-given-class-in-java
              java.lang.reflect.Array.newInstance(elmClass, 0).getClass
          }
        case _ => ResolutionError.IllegalType(tpe, loc).toFailure
      }

    case Type.Cst(TypeConstructor.Vector) =>
      tpe.typeArguments match {
        case elmTyp :: Nil =>
          mapN(getJVMType(elmTyp, loc)) {
            case elmClass =>
              // See: https://stackoverflow.com/questions/1679421/how-to-get-the-array-class-for-a-given-class-in-java
              java.lang.reflect.Array.newInstance(elmClass, 0).getClass
          }
        case _ => ResolutionError.IllegalType(tpe, loc).toFailure
      }

    case Type.Cst(TypeConstructor.Native(clazz)) => clazz.toSuccess

    case Type.RecordEmpty => Class.forName("java.lang.Object").toSuccess

    case Type.RecordExtend(_, _, _) => Class.forName("java.lang.Object").toSuccess

    case Type.SchemaEmpty => Class.forName("java.lang.Object").toSuccess

    case Type.SchemaExtend(_, _, _) => Class.forName("java.lang.Object").toSuccess

    case _ => ResolutionError.IllegalType(tpe, loc).toFailure
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

}
