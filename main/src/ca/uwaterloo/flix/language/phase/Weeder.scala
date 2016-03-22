package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

/**
  * The Weeder phase performs simple syntactic checks and rewritings.
  */
object Weeder {

  import WeederError._

  /**
    * A common super-type for weeding errors.
    */
  sealed trait WeederError extends CompilationError

  object WeederError {

    // TODO: Sort errors by alphabetical.

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate that the alias `name` was defined multiple times.
      *
      * @param name the name of the variable.
      * @param loc1 the location of the first declaration.
      * @param loc2 the location of the second declaration.
      */
    case class DuplicateAlias(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate definition of the variable '$name'.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the aliases.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the annotation `name` was used multiple times.
      *
      * @param name the name of the attribute.
      * @param loc1 the location of the first use.
      * @param loc2 the location of the second use.
      */
    case class DuplicateAnnotation(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate annotation '$name'.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Remove one of the annotations.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the attribute `name` was declared multiple times.
      *
      * @param name the name of the attribute.
      * @param loc1 the location of the first declaration.
      * @param loc2 the location of the second declaration.
      */
    case class DuplicateAttribute(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate attribute name '$name'.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the attributes.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the formal argument `name` was declared multiple times.
      *
      * @param name the name of the argument.
      * @param loc1 the location of the first declaration.
      * @param loc2 the location of the second declaration.
      */
    case class DuplicateFormal(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate formal argument '$name'.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the arguments.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the tag `name` was declared multiple times.
      *
      * @param name the name of the tag.
      * @param loc1 the location of the first declaration.
      * @param loc2 the location of the second declaration.
      */
    case class DuplicateTag(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate tag name '$name'.")}
           |
            |First declaration was here:
           |${loc1.underline}
           |Second declaration was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the tags.
         """.stripMargin
    }

    /**
      * An error raised to indicate that an index declaration defines no indexes.
      *
      * @param loc the location where the index declaration occurs.
      */
    case class MissingIndex(loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Missing index. Must declare at least one index.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate that an index declaration defines an index on zero attributes.
      *
      * @param loc the location where the illegal index occurs.
      */
    case class IllegalIndex(loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal index. An index must select at least one attribute.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate that a predicate is not allowed in the head of a fact/rule.
      *
      * @param loc the location where the illegal predicate occurs.
      */
    case class IllegalHeadPredicate(loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal predicate in the head of a fact/rule.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate the presence of an illegal annotation.
      *
      * @param name the name of the illegal annotation.
      * @param loc  the location of the annotation.
      */
    case class IllegalAnnotation(name: String, loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> Illegal annotation '$name'.")}
           |
           |${loc.underline}
           |
         """.stripMargin
    }

    /**
      * An error raised to indicate that an illegal term occurs in a body predicate.
      *
      * @param msg the error message.
      * @param loc the location where the illegal term occurs.
      */
    case class IllegalBodyTerm(msg: String, loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal term in the body of a rule.")}
           |
            |${loc.underline}
           |$msg
         """.stripMargin
    }

    /**
      * An error raised to indicate that an illegal term occurs in a head predicate.
      *
      * @param msg the error message.
      * @param loc the location where the illegal term occurs.
      */
    case class IllegalHeadTerm(msg: String, loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal term in the head of a fact/rule.")}
           |
            |${loc.underline}
           |$msg
         """.stripMargin
    }

    /**
      * An error raised to indicate an illegal bounded lattice definition.
      *
      * @param loc the location where the illegal definition occurs.
      */
    case class IllegalLattice(loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Lattice definition must have exactly five components: bot, top, leq, lub and glb.")}
           |
            |${loc.underline}
           |the 1st component must be the bottom element,
           |the 2nd component must be the top element,
           |the 3rd component must be the partial order function,
           |the 4th component must be the least upper bound function, and
           |the 5th component must be the greatest upper bound function.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the variable `name` occurs multiple times in the same pattern.
      *
      * @param name the name of the variable.
      * @param loc1 the location of the first use of the variable.
      * @param loc2 the location of the second use of the variable.
      */
    case class NonLinearPattern(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate definition of the same variable '$name' in pattern.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |
            |A variable is must only occurs once in a pattern.
           |
            |Tip: Remove the duplicate variable and use '==' to test for equality.
         """.stripMargin
    }

    /**
      * An error raised to indicate that a syntactic construct, although successfully parsed, is currently not supported.
      *
      * @param msg the error message.
      * @param loc the location of the syntactic construct.
      */
    case class Unsupported(msg: String, loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Unsupported feature: $msg")}
           |
            |${loc.underline}
           |This feature is not yet supported, implemented or considered stable.
           |
            |Tip: Avoid using this feature.
         """.stripMargin
    }

  }

  /**
    * Compiles the given parsed `past` to a weeded ast.
    */
  def weed(past: ParsedAst.Program, hooks: Map[Symbol.Resolved, Ast.Hook]): Validation[WeededAst.Root, WeederError] = {
    val b = System.nanoTime()
    val declarations = past.roots.flatMap(_.decls)
    @@(declarations.map(Declaration.compile)) map {
      case decls =>
        val e = System.nanoTime()
        WeededAst.Root(decls, hooks, past.time.copy(weeder = e - b))
    }
  }

  object Declaration {

    /**
      * Compiles the given parsed declaration `past` to a weeded declaration.
      */
    // TODO: Inline

    def compile(past: ParsedAst.Declaration): Validation[WeededAst.Declaration, WeederError] = past match {
      case d: ParsedAst.Declaration.Namespace => Declaration.compile(d)
      case d: ParsedAst.Declaration.Definition => Declaration.compile(d)
      case ParsedAst.Declaration.External(sp1, ident, formals, tpe, sp2) => ???
      case ParsedAst.Declaration.Law(sp1, ident, tparams, params, tpe, body, sp2) => ???
      case d: ParsedAst.Declaration.Enum => Declaration.compile(d)
      case ParsedAst.Declaration.Class(sp1, ident, tparams, bounds, body, sp2) => ???
      case ParsedAst.Declaration.Impl(sp1, ident, tparams, bounds, body, sp2) => ???
      case d: ParsedAst.Declaration.BoundedLattice => Declaration.compile(d)
      case d: ParsedAst.Declaration.Relation => Declaration.compile(d)
      case d: ParsedAst.Declaration.Lattice => Declaration.compile(d)
      case d: ParsedAst.Declaration.Fact => Declaration.compile(d)
      case d: ParsedAst.Declaration.Rule => Declaration.compile(d)
      case d: ParsedAst.Declaration.Index => Declaration.compile(d)
    }

    /**
      * Compiles the given parsed namespace declaration `past` to a weeded namespace declaration.
      */
    def compile(past: ParsedAst.Declaration.Namespace): Validation[WeededAst.Declaration.Namespace, WeederError] =
      @@(past.body.map(compile)) map {
        case decls => WeededAst.Declaration.Namespace(past.name, decls, mkSL(past.sp1, past.sp2))
      }

    /**
      * Compiles the given parsed fact `past` to a weeded fact.
      */
    def compile(past: ParsedAst.Declaration.Fact): Validation[WeededAst.Declaration.Fact, WeederError] =
      Predicate.Head.compile(past.head) map {
        case p => WeededAst.Declaration.Fact(p, mkSL(past.sp1, past.sp2))
      }

    /**
      * Compiles the parsed rule `past` to a weeded rule.
      */
    def compile(past: ParsedAst.Declaration.Rule): Validation[WeededAst.Declaration.Rule, WeederError] = {
      // compute an map from variable names to alias predicates.
      val aliases = past.body collect {
        case p: ParsedAst.Predicate.Equal => p
      }
      val aliasesVal = Validation.fold[ParsedAst.Predicate.Equal, Map[String, ParsedAst.Predicate.Equal], WeederError](aliases, Map.empty) {
        case (m, p) => m.get(p.ident.name) match {
          case None => (m + (p.ident.name -> p)).toSuccess
          case Some(otherAlias) => DuplicateAlias(p.ident.name, mkSL(otherAlias.sp1, otherAlias.sp2), mkSL(p.sp1, p.sp2)).toFailure
        }
      }

      aliasesVal flatMap {
        case aliases =>
          val headVal = Predicate.Head.compile(past.head, aliases)
          val bodyVal = @@(past.body.filterNot(_.isInstanceOf[ParsedAst.Predicate.Equal]).map(Predicate.Body.compile))

          @@(headVal, bodyVal) map {
            case (head, body) => WeededAst.Declaration.Rule(head, body, mkSL(past.sp1, past.sp2))
          }
      }
    }

    /**
      * Compiles the given parsed function declaration `past` to a weeded definition.
      */
    def compile(past: ParsedAst.Declaration.Definition): Validation[WeededAst.Definition.Constant, WeederError] = {
      val annotationsVal = Annotations.compile(past.annotations)

      // TODO: Need to move certain annotations to each lattice valued argument...?

      // check duplicate formals.
      val seen = mutable.Map.empty[String, Name.Ident]
      val formalsVal = @@(past.formals.map {
        case ParsedAst.FormalArg(ident, annotations, tpe) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            WeededAst.FormalArg(ident, tpe).toSuccess
          case Some(otherIdent) =>
            DuplicateFormal(ident.name, otherIdent.loc, ident.loc).toFailure
        }
      })

      @@(annotationsVal, formalsVal, Expression.compile(past.exp)) map {
        case (anns, args, exp) =>
          val tpe = Type.Lambda(args map (_.tpe), past.tpe)
          WeededAst.Definition.Constant(past.ident, args, exp, tpe, mkSL(past.sp1, past.sp2))
      }
    }

    /**
      * Compiles the given parsed enum declaration `past` to a weeded enum definition.
      *
      * Returns [[Failure]] if the same tag name occurs twice.
      */
    def compile(past: ParsedAst.Declaration.Enum): Validation[WeededAst.Definition.Enum, WeederError] = {
      // check duplicate tags.
      Validation.fold[ParsedAst.Case, Map[String, Type.UnresolvedTag], WeederError](past.cases, Map.empty) {
        case (macc, caze: ParsedAst.Case) =>
          val tagName = caze.ident.name
          macc.get(tagName) match {
            case None => (macc + (tagName -> Type.UnresolvedTag(past.ident, caze.ident, caze.tpe))).toSuccess
            case Some(otherTag) => DuplicateTag(tagName, otherTag.tag.loc, mkSL(caze.sp1, caze.sp2)).toFailure
          }
      } map {
        case m => WeededAst.Definition.Enum(past.ident, m, mkSL(past.sp1, past.sp2))
      }
    }

    /**
      * Compiles the given parsed lattice `past` to a weeded lattice definition.
      */
    def compile(past: ParsedAst.Declaration.BoundedLattice): Validation[WeededAst.Definition.BoundedLattice, WeederError] = {
      // check lattice definition.
      val elmsVal = @@(past.elms.toList.map(Expression.compile))
      elmsVal flatMap {
        case List(bot, top, leq, lub, glb) => WeededAst.Definition.BoundedLattice(past.tpe, bot, top, leq, lub, glb, mkSL(past.sp1, past.sp2)).toSuccess
        case _ => IllegalLattice(mkSL(past.sp1, past.sp2)).toFailure
      }
    }

    /**
      * Compiles the given parsed relation `past` to a weeded relation definition.
      */
    def compile(past: ParsedAst.Declaration.Relation): Validation[WeededAst.Table.Relation, WeederError] = {
      // check for duplicate attributes.
      val seen = mutable.Map.empty[String, Name.Ident]
      val attributesVal = past.attributes.map {
        case ParsedAst.Attribute(ident, tpe) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            WeededAst.Attribute(ident, tpe).toSuccess
          case Some(otherIdent) =>
            DuplicateAttribute(ident.name, otherIdent.loc, ident.loc).toFailure
        }
      }

      @@(attributesVal) map {
        case attributes => WeededAst.Table.Relation(past.ident, attributes, mkSL(past.sp1, past.sp2))
      }
    }

    /**
      * Compiles the given parsed relation `past` to a weeded lattice definition.
      */
    def compile(past: ParsedAst.Declaration.Lattice): Validation[WeededAst.Table.Lattice, WeederError] = {
      // check for duplicate attributes.
      val seen = mutable.Map.empty[String, Name.Ident]
      val attributesVal = past.attributes.map {
        case ParsedAst.Attribute(ident, tpe) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            WeededAst.Attribute(ident, tpe).toSuccess
          case Some(otherIdent) =>
            DuplicateAttribute(ident.name, otherIdent.loc, ident.loc).toFailure
        }
      }

      @@(attributesVal) flatMap {
        case attributes => WeededAst.Table.Lattice(past.ident, attributes.init, attributes.last, mkSL(past.sp1, past.sp2)).toSuccess
      }
    }

    /**
      * Compiles the given parsed index definition `past` to a weeded index definition.
      */
    def compile(past: ParsedAst.Declaration.Index): Validation[WeededAst.Definition.Index, WeederError] = {
      if (past.indexes.isEmpty)
        return MissingIndex(mkSL(past.sp1, past.sp2)).toFailure

      if (past.indexes.exists(_.isEmpty)) {
        return IllegalIndex(mkSL(past.sp1, past.sp2)).toFailure
      }

      WeededAst.Definition.Index(past.ident, past.indexes, mkSL(past.sp1, past.sp2)).toSuccess
    }

  }

  object Literals {
    /**
      * Compiles the parsed literal `past` to a weeded literal.
      */
    // TODO: check bounds etc
    def compile(past: ParsedAst.Literal): Validation[WeededAst.Literal, WeederError] = past match {
      case ParsedAst.Literal.Unit(sp1, sp2) =>
        WeededAst.Literal.Unit(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Bool(sp1, lit, sp2) => lit match {
        case "true" => WeededAst.Literal.Bool(lit = true, mkSL(sp1, sp2)).toSuccess
        case "false" => WeededAst.Literal.Bool(lit = false, mkSL(sp1, sp2)).toSuccess
        case _ => throw InternalCompilerException("Non-true/false boolean.")
      }

      case ParsedAst.Literal.Char(sp1, lit, sp2) =>
        WeededAst.Literal.Char(lit(0), mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
        val s = if (sign) s"-$before.$after" else s"$before.$after"
        WeededAst.Literal.Float32(s.toFloat, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
        val s = if (sign) s"-$before.$after" else s"$before.$after"
        WeededAst.Literal.Float64(s.toDouble, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Int8(sp1, sign, lit, sp2) =>
        val s = if (sign) "-" + lit else lit
        WeededAst.Literal.Int8(s.toByte, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Int16(sp1, sign, lit, sp2) =>
        val s = if (sign) "-" + lit else lit
        WeededAst.Literal.Int16(s.toShort, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Int32(sp1, sign, lit, sp2) =>
        val s = if (sign) "-" + lit else lit
        WeededAst.Literal.Int32(s.toInt, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Int64(sp1, sign, lit, sp2) =>
        val s = if (sign) "-" + lit else lit
        WeededAst.Literal.Int64(s.toLong, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Str(sp1, lit, sp2) =>
        WeededAst.Literal.Str(lit, mkSL(sp1, sp2)).toSuccess
    }
  }

  object Expression {
    /**
      * Compiles the parsed expression `past` to a weeded expression.
      */
    def compile(past: ParsedAst.Expression): Validation[WeededAst.Expression, WeederError] = past match {
      case ParsedAst.Expression.Lit(sp1, lit, sp2) =>
        Literals.compile(lit) map {
          case lit => WeededAst.Expression.Lit(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Var(sp1, name, sp2) =>
        WeededAst.Expression.Var(name, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.Apply(sp1, lambda, actuals, sp2) =>
        @@(compile(lambda), @@(actuals map compile)) map {
          case (lambda, args) => WeededAst.Expression.Apply(lambda, args, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
        @@(compile(exp1), compile(exp2)) map {
          case (e1, e2) =>
            val loc = mkSL(exp1.leftMostSourcePosition, sp2)
            WeededAst.Expression.Apply(WeededAst.Expression.Var(name, loc), List(e1, e2), loc)
        }

      case ParsedAst.Expression.Lambda(sp1, formals, body, sp2) => ??? // TODO

      case ParsedAst.Expression.Unary(sp1, op, exp, sp2) => compile(exp) map {
        case e => WeededAst.Expression.Unary(op, e, mkSL(sp1, sp2))
      }

      case ParsedAst.Expression.Binary(exp1, op, exp2, sp2) =>
        @@(compile(exp1), compile(exp2)) map {
          case (e1, e2) => WeededAst.Expression.Binary(op, e1, e2, mkSL(exp1.leftMostSourcePosition, sp2))
        }

      case ParsedAst.Expression.ExtendedBinary(exp1, op, exp2, sp2) =>
        @@(compile(exp1), compile(exp2)) map {
          case (e1, e2) =>
            op match {
              case ExtBinaryOperator.Leq =>
                val sp1 = exp1.leftMostSourcePosition
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "⊑", sp2)
                val namespace = Name.NName(exp1.leftMostSourcePosition, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

              case ExtBinaryOperator.Lub =>
                val sp1 = exp1.leftMostSourcePosition
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "⊔", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

              case ExtBinaryOperator.Glb =>
                val sp1 = exp1.leftMostSourcePosition
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "⊓", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

              case ExtBinaryOperator.Widen =>
                val sp1 = exp1.leftMostSourcePosition
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "▽", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

              case ExtBinaryOperator.Narrow =>
                val sp1 = exp1.leftMostSourcePosition
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "△", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)
            }
        }

      case ParsedAst.Expression.LetMatch(sp1, pat, value, body, sp2) =>
        // Compiles a let-match to either a regular let-binding or a pattern match.
        @@(Patterns.compile(pat), compile(value), compile(body)) map {
          case (WeededAst.Pattern.Var(ident, loc), value, body) =>
            WeededAst.Expression.Let(ident, value, body, mkSL(sp1, sp2))
          case (pattern, value, body) =>
            val rules = List(pattern -> body)
            WeededAst.Expression.Match(value, rules, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.IfThenElse(sp1, exp1, exp2, exp3, sp2) =>
        @@(compile(exp1), compile(exp2), compile(exp3)) map {
          case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Switch(sp1, rules, sp2) =>
        val rulesVal = rules map {
          case (cond, body) => @@(Expression.compile(cond), Expression.compile(body))
        }
        @@(rulesVal) map {
          case rules => WeededAst.Expression.Switch(rules, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Match(sp1, exp, rules, sp2) =>
        val rulesVal = rules map {
          case (pat, body) => @@(Patterns.compile(pat), compile(body))
        }
        @@(compile(exp), @@(rulesVal)) map {
          case (e, rs) => WeededAst.Expression.Match(e, rs, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Tag(sp1, enum, tag, o, sp2) => o match {
        case None =>
          val loc = mkSL(sp1, sp2)
          val lit = WeededAst.Literal.Unit(loc)
          val exp = WeededAst.Expression.Lit(lit, loc)
          WeededAst.Expression.Tag(enum, tag, exp, loc).toSuccess
        case Some(exp) => compile(exp) map {
          case e => WeededAst.Expression.Tag(enum, tag, e, mkSL(sp1, sp2))
        }
      }

      case ParsedAst.Expression.Tuple(sp1, elms, sp2) =>
        @@(elms map compile) map {
          case Nil =>
            val loc = mkSL(sp1, sp2)
            val lit = WeededAst.Literal.Unit(loc)
            WeededAst.Expression.Lit(lit, loc)
          case x :: Nil => x
          case xs => WeededAst.Expression.Tuple(xs, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.FNone(sp1, sp2) => ???

      case ParsedAst.Expression.FSome(sp1, elm, sp2) => ???

      case ParsedAst.Expression.FNil(sp1, sp2) => ???

      case ParsedAst.Expression.FList(hd, tl, sp2) => ???

      case ParsedAst.Expression.FSet(sp1, elms, sp2) =>
        @@(elms map compile) map {
          case elms => WeededAst.Expression.Set(elms, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.FMap(sp1, elms, sp2) => ???

      case ParsedAst.Expression.Ascribe(sp1, exp, tpe, sp2) =>
        compile(exp) map {
          case e => WeededAst.Expression.Ascribe(e, tpe, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.UserError(sp1, tpe, sp2) =>
        WeededAst.Expression.Error(tpe, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.Bot(sp1, sp2) =>
        val ident = Name.Ident(sp1, "⊥", sp2)
        val namespace = Name.NName(sp1, List.empty, sp2)
        val name = Name.QName(sp1, namespace, ident, sp2)
        val lambda = WeededAst.Expression.Var(name, mkSL(sp1, sp2))
        WeededAst.Expression.Apply(lambda, List(), mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.Top(sp1, sp2) =>
        val ident = Name.Ident(sp1, "⊤", sp2)
        val namespace = Name.NName(sp1, List.empty, sp2)
        val name = Name.QName(sp1, namespace, ident, sp2)
        val lambda = WeededAst.Expression.Var(name, mkSL(sp1, sp2))
        WeededAst.Expression.Apply(lambda, List(), mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.Existential(sp1, params, body, sp2) => ???

      case ParsedAst.Expression.Universal(sp1, params, body, sp2) => ???

    }
  }

  object Patterns {
    /**
      * Compiles the parsed pattern `past`.
      */
    def compile(past: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = {
      // check non-linear pattern, i.e. duplicate variable occurrence.
      val seen = mutable.Map.empty[String, Name.Ident]

      def visit(p: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = p match {
        case ParsedAst.Pattern.Wildcard(sp1, sp2) => WeededAst.Pattern.Wildcard(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Pattern.Var(sp1, ident, sp2) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            WeededAst.Pattern.Var(ident, mkSL(sp1, sp2)).toSuccess
          case Some(otherIdent) =>
            NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
        }

        case ParsedAst.Pattern.Lit(sp1, plit, sp2) => Literals.compile(plit) map {
          case lit => WeededAst.Pattern.Lit(lit, mkSL(sp1, sp2))
        }

        case ParsedAst.Pattern.Tag(sp1, enum, tag, o, sp2) => o match {
          case None =>
            val loc = mkSL(sp1, sp2)
            val lit = WeededAst.Literal.Unit(loc)
            val pat = WeededAst.Pattern.Lit(lit, loc)
            WeededAst.Pattern.Tag(enum, tag, pat, loc).toSuccess
          case Some(ppat) => visit(ppat) map {
            case pat => WeededAst.Pattern.Tag(enum, tag, pat, mkSL(sp1, sp2))
          }
        }

        case ParsedAst.Pattern.Tuple(sp1, pats, sp2) => @@(pats map visit) map {
          case Nil => WeededAst.Pattern.Lit(WeededAst.Literal.Unit(mkSL(sp1, sp2)), mkSL(sp1, sp2))
          case x :: Nil => x
          case xs => WeededAst.Pattern.Tuple(xs, mkSL(sp1, sp2))
        }

        case ParsedAst.Pattern.FNil(sp1, sp2) => ???

        case ParsedAst.Pattern.FList(p1, p2, sp2) =>
          @@(compile(p1), compile(p2)) map {
            case (hd, tl) => WeededAst.Pattern.List(hd, tl, mkSL(p1.leftMostSourcePosition, sp2))
          }

        case ParsedAst.Pattern.FNone(sp1, sp2) => ???

        case ParsedAst.Pattern.FSome(sp1, pat, sp2) => ???

        case ParsedAst.Pattern.FSet(sp1, elms, rest, sp2) => ???

        case ParsedAst.Pattern.FMap(sp1, elms, rest, sp2) => ???
      }

      visit(past)
    }
  }


  object Predicate {

    object Head {

      /**
        * Compiles the given parsed predicate `p` to a weeded head predicate.
        */
      def compile(past: ParsedAst.Predicate, aliases: Map[String, ParsedAst.Predicate.Equal] = Map.empty): Validation[WeededAst.Predicate.Head, WeederError] = past match {
        case p: ParsedAst.Predicate.Ambiguous =>
          @@(p.terms.map(t => Term.Head.compile(t, aliases))) map {
            case terms => WeededAst.Predicate.Head.Relation(p.name, terms, mkSL(p.sp1, p.sp2))
          }

        case ParsedAst.Predicate.Equal(sp1, ident, term, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.Loop(sp1, ident, term, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.NotEqual(sp1, ident1, ident2, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
      }

    }

    object Body {

      /**
        * Compiles the given parsed predicate `p` to a weeded body predicate.
        */
      def compile(past: ParsedAst.Predicate): Validation[WeededAst.Predicate.Body, WeederError] = past match {
        case p: ParsedAst.Predicate.Ambiguous =>
          @@(p.terms.map(Term.Body.compile)) map {
            case terms => WeededAst.Predicate.Body.Ambiguous(p.name, terms, mkSL(p.sp1, p.sp2))
          }

        case p: ParsedAst.Predicate.NotEqual =>
          WeededAst.Predicate.Body.NotEqual(p.ident1, p.ident2, mkSL(p.sp1, p.sp2)).toSuccess

        case p: ParsedAst.Predicate.Loop => Term.Head.compile(p.term, Map.empty) map {
          case term => WeededAst.Predicate.Body.Loop(p.ident, term, mkSL(p.sp1, p.sp2))
        }

        case p: ParsedAst.Predicate.Equal => throw InternalCompilerException("Alias predicate should already have been eliminated.")
      }
    }

  }

  object Term {

    object Head {

      /**
        * Compiles the given parsed head term `past` to a weeded term.
        */
      def compile(past: ParsedAst.Term, aliases: Map[String, ParsedAst.Predicate.Equal]): Validation[WeededAst.Term.Head, WeederError] = past match {
        case ParsedAst.Term.Wildcard(sp1, sp2) => IllegalHeadTerm("Wildcards may not occur in head predicates.", mkSL(sp1, sp2)).toFailure

        case ParsedAst.Term.Var(sp1, ident, sp2) => aliases.get(ident.name) match {
          case None => WeededAst.Term.Head.Var(ident, mkSL(sp1, sp2)).toSuccess
          case Some(alias) => compile(alias.term, aliases)
        }

        case ParsedAst.Term.Lit(sp1, lit, sp2) => Literals.compile(lit) map {
          case lit => WeededAst.Term.Head.Lit(lit, mkSL(sp1, sp2))
        }

        case ParsedAst.Term.Tag(sp1, enum, tag, t, sp2) => t match {
          case None =>
            val loc = mkSL(sp1, sp2)
            val unit = WeededAst.Term.Head.Lit(WeededAst.Literal.Unit(loc), loc)
            WeededAst.Term.Head.Tag(enum, tag, unit, loc).toSuccess
          case Some(t) => compile(t, aliases) map {
            case inner => WeededAst.Term.Head.Tag(enum, tag, inner, mkSL(sp1, sp2))
          }
        }

        case ParsedAst.Term.Tuple(sp1, elms, sp2) => elms.toList match {
          case Nil =>
            val loc = mkSL(sp1, sp2)
            WeededAst.Term.Head.Lit(WeededAst.Literal.Unit(loc), loc).toSuccess
          case x :: Nil => compile(x, aliases)
          case xs => @@(xs map (x => compile(x, aliases))) map {
            case elms => WeededAst.Term.Head.Tuple(elms, mkSL(sp1, sp2))
          }
        }

        case ParsedAst.Term.Apply(sp1, name, args, sp2) =>
          @@(args map (t => compile(t, aliases))) map {
            case args => WeededAst.Term.Head.Apply(name, args, mkSL(sp1, sp2))
          }
      }
    }

    object Body {
      /**
        * Compiles the given parsed body term `past` to a weeded term.
        */
      def compile(past: ParsedAst.Term): Validation[WeededAst.Term.Body, WeederError] = past match {
        case ParsedAst.Term.Wildcard(sp1, sp2) => WeededAst.Term.Body.Wildcard(mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Term.Var(sp1, ident, sp2) => WeededAst.Term.Body.Var(ident, mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Term.Lit(sp1, lit, sp2) => Literals.compile(lit) map {
          case lit => WeededAst.Term.Body.Lit(lit, mkSL(sp1, sp2))
        }
        case ParsedAst.Term.Tag(sp1, enum, tag, t, sp2) => IllegalBodyTerm("Tags may not occur in body predicates.", mkSL(sp1, sp2)).toFailure
        case ParsedAst.Term.Tuple(sp1, elms, sp2) => IllegalBodyTerm("Tuples may not occur in body predicates.", mkSL(sp1, sp2)).toFailure
        case ParsedAst.Term.Apply(sp1, name, args, sp2) => IllegalBodyTerm("Function calls may not occur in body predicates.", mkSL(sp1, sp2)).toFailure
      }
    }

  }

  object Annotations {
    /**
      * Weeds the given sequence of parsed annotation `xs`.
      */
    def compile(xs: Seq[ParsedAst.Annotation]): Validation[Ast.Annotations, WeederError] = {
      // track seen annotations.
      val seen = mutable.Map.empty[String, ParsedAst.Annotation]

      // loop through each annotation.
      val result = xs.toList map {
        case x => seen.get(x.name) match {
          case None =>
            seen += (x.name -> x)
            Annotations.compile(x)
          case Some(otherAnn) =>
            DuplicateAnnotation(x.name, mkSL(otherAnn.sp1, otherAnn.sp2), mkSL(x.sp1, x.sp2)).toFailure
        }
      }
      @@(result) map Ast.Annotations
    }

    /**
      * Weeds the given parsed annotation `past`.
      */
    def compile(past: ParsedAst.Annotation): Validation[Ast.Annotation, WeederError] = {
      val loc = mkSL(past.sp1, past.sp2)
      past.name match {
        case "associative" => Ast.Annotation.Associative(loc).toSuccess
        case "commutative" => Ast.Annotation.Commutative(loc).toSuccess
        case "monotone" => Ast.Annotation.Monotone(loc).toSuccess
        case "strict" => Ast.Annotation.Strict(loc).toSuccess
        case "unchecked" => Ast.Annotation.Unchecked(loc).toSuccess
        case "unsafe" => Ast.Annotation.Unsafe(loc).toSuccess
        case _ => IllegalAnnotation(past.name, loc).toFailure
      }
    }
  }

  /**
    * Alias for SourceLocation.mk
    */
  private def mkSL(sp1: SourcePosition, sp2: SourcePosition): SourceLocation = SourceLocation.mk(sp1, sp2)
}
