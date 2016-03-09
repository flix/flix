package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.util.Validation
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
      * An error raised to indicate an illegal lattice attribute occurring in a relation.
      *
      * @param loc the location where the illegal definition occurs.
      */
    case class IllegalLatticeAttributeInRelation(loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal lattice attribute in relation.")}
           |
            |${loc.underline}
           |A relation must not contain any lattice interpreted attributes.
           |
            |Tip: Remove the lattice interpretation (<>) or change the relation to a lattice (replace rel by lat).
         """.stripMargin
    }

    /**
      * An error raised to indicate that the last attribute of a lattice definition does not have a lattice interpretation.
      *
      * @param loc the location where the illegal definition occurs.
      */
    case class IllegalNonLatticeAttribute(loc: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal non-lattice attribute.")}
           |
            |${loc.underline}
           |The last attribute of lattice must have a lattice interpretation.
           |
            |Tip: Change the interpretation of the last attribute (adding <>) or change the lattice to a relation (replace lat by rel).
         """.stripMargin
    }

    /**
      * An error raised to indicate that a lattice attribute is followed by a non-lattice attribute.
      *
      * @param loc1 the location where the first lattice attribute occurs.
      * @param loc2 the location where a following non-lattice attribute occurs.
      */
    case class IllegalMixedAttributes(loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val message =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal non-lattice attribute follows lattice attribute.")}
           |
           |The first lattice attribute was here:
           |${loc1.underline}
           |The illegal non-lattice attribute was here:
           |${loc2.underline}
           |
            |Tip: Rearrange the attributes or possibly their interpretations.
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
  def weed(past: ParsedAst.Root, hooks: Map[Symbol.Resolved, Ast.Hook]): Validation[WeededAst.Root, WeederError] = {
    val b = System.nanoTime()
    @@(past.declarations.map(Declaration.compile)) map {
      case decls =>
        val e = System.nanoTime()
        WeededAst.Root(decls, hooks, past.time.copy(weeder = e - b))
    }
  }

  object Declaration {

    /**
      * Compiles the given parsed declaration `past` to a weeded declaration.
      */
    def compile(past: ParsedAst.Declaration): Validation[WeededAst.Declaration, WeederError] = past match {
      case d: ParsedAst.Declaration.Namespace => Declaration.compile(d)
      case d: ParsedAst.Declaration.Fact => Declaration.compile(d)
      case d: ParsedAst.Declaration.Rule => Declaration.compile(d)
      case d: ParsedAst.Definition => Definition.compile(d)
    }

    /**
      * Compiles the given parsed namespace declaration `past` to a weeded namespace declaration.
      */
    def compile(past: ParsedAst.Declaration.Namespace): Validation[WeededAst.Declaration.Namespace, WeederError] =
      @@(past.body.map(compile)) map {
        case decls => WeededAst.Declaration.Namespace(past.name, decls, past.loc)
      }

    /**
      * Compiles the given parsed fact `past` to a weeded fact.
      */
    def compile(past: ParsedAst.Declaration.Fact): Validation[WeededAst.Declaration.Fact, WeederError] =
      Predicate.Head.compile(past.head) map {
        case p => WeededAst.Declaration.Fact(p, past.loc)
      }

    /**
      * Compiles the parsed rule `past` to a weeded rule.
      */
    def compile(past: ParsedAst.Declaration.Rule): Validation[WeededAst.Declaration.Rule, WeederError] = {
      // compute an map from variable names to alias predicates.
      val aliasesVal = Validation.fold[ParsedAst.Predicate.Alias, Map[String, ParsedAst.Predicate.Alias], WeederError](past.aliases, Map.empty) {
        case (m, p) => m.get(p.ident.name) match {
          case None => (m + (p.ident.name -> p)).toSuccess
          case Some(otherAlias) => DuplicateAlias(p.ident.name, otherAlias.loc, p.loc).toFailure
        }
      }

      aliasesVal flatMap {
        case aliases =>
          val headVal = Predicate.Head.compile(past.head, aliases)
          val bodyVal = @@(past.body.filterNot(_.isInstanceOf[ParsedAst.Predicate.Alias]).map(Predicate.Body.compile))

          @@(headVal, bodyVal) map {
            case (head, body) => WeededAst.Declaration.Rule(head, body, past.loc)
          }
      }
    }

  }

  object Definition {

    /**
      * Compiles the given parsed definition `past` to a weeded definition.
      */
    // TODO: Inline all calls into this function...
    def compile(past: ParsedAst.Definition): Validation[WeededAst.Declaration, WeederError] = past match {
      case d: ParsedAst.Definition.Function => Definition.compile(d)
      case d: ParsedAst.Definition.Enum => Definition.compile(d)
      case d: ParsedAst.Definition.BoundedLattice => Definition.compile(d)
      case d: ParsedAst.Definition.Relation => Definition.compile(d)
      case d: ParsedAst.Definition.Lattice => Definition.compile(d)
      case d: ParsedAst.Definition.Index => Definition.compile(d)
    }

    /**
      * Compiles the given parsed function declaration `past` to a weeded definition.
      */
    def compile(past: ParsedAst.Definition.Function): Validation[WeededAst.Definition.Constant, WeederError] = {
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

      @@(annotationsVal, formalsVal, Expression.compile(past.body)) map {
        case (anns, args, body) =>
          val exp = WeededAst.Expression.Lambda(anns, args, body, past.tpe, past.body.loc)
          val tpe = Type.Lambda(args map (_.tpe), past.tpe)
          WeededAst.Definition.Constant(past.ident, exp, tpe, past.loc)
      }
    }

    /**
      * Compiles the given parsed enum declaration `past` to a weeded enum definition.
      *
      * Returns [[Failure]] if the same tag name occurs twice.
      */
    def compile(past: ParsedAst.Definition.Enum): Validation[WeededAst.Definition.Enum, WeederError] = {
      // check duplicate tags.
      Validation.fold[ParsedAst.Case, Map[String, Type.UnresolvedTag], WeederError](past.cases, Map.empty) {
        case (macc, caze: ParsedAst.Case) =>
          val tagName = caze.ident.name
          macc.get(tagName) match {
            case None => (macc + (tagName -> Type.UnresolvedTag(past.ident, caze.ident, caze.tpe))).toSuccess
            case Some(otherTag) => DuplicateTag(tagName, otherTag.tag.loc, caze.loc).toFailure
          }
      } map {
        case m => WeededAst.Definition.Enum(past.ident, m, past.loc)
      }
    }

    /**
      * Compiles the given parsed lattice `past` to a weeded lattice definition.
      */
    def compile(past: ParsedAst.Definition.BoundedLattice): Validation[WeededAst.Definition.BoundedLattice, WeederError] = {
      // check lattice definition.
      val elmsVal = @@(past.elms.toList.map(Expression.compile))
      elmsVal flatMap {
        case List(bot, top, leq, lub, glb) => WeededAst.Definition.BoundedLattice(past.tpe, bot, top, leq, lub, glb, past.loc).toSuccess
        case _ => IllegalLattice(past.loc).toFailure
      }
    }

    /**
      * Compiles the given parsed relation `past` to a weeded relation definition.
      */
    def compile(past: ParsedAst.Definition.Relation): Validation[WeededAst.Table.Relation, WeederError] = {
      // check duplicate attributes.
      val seen = mutable.Map.empty[String, Name.Ident]
      val attributesVal = past.attributes.map {
        case ParsedAst.Attribute(ident, interp) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            interp match {
              case i: ParsedAst.Interpretation.Set =>
                WeededAst.Attribute(ident, interp.tpe, WeededAst.Interpretation.Set).toSuccess
              case i: ParsedAst.Interpretation.Lattice =>
                IllegalLatticeAttributeInRelation(ident.loc).toFailure
            }
          case Some(otherIdent) =>
            DuplicateAttribute(ident.name, otherIdent.loc, ident.loc).toFailure
        }
      }

      @@(attributesVal) map {
        case attributes => WeededAst.Table.Relation(past.ident, attributes, past.loc)
      }
    }

    /**
      * Compiles the given parsed relation `past` to a weeded lattice definition.
      */
    def compile(past: ParsedAst.Definition.Lattice): Validation[WeededAst.Table.Lattice, WeederError] = {
      // TODO: Rewrite so we can get rid of WeededAst.Interpretation.

      // check duplicate attributes.
      val seen = mutable.Map.empty[String, Name.Ident]
      val attributesVal = past.attributes.map {
        case ParsedAst.Attribute(ident, interp) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            interp match {
              case i: ParsedAst.Interpretation.Set =>
                WeededAst.Attribute(ident, interp.tpe, WeededAst.Interpretation.Set).toSuccess
              case i: ParsedAst.Interpretation.Lattice =>
                WeededAst.Attribute(ident, interp.tpe, WeededAst.Interpretation.Lattice).toSuccess
            }
          case Some(otherIdent) =>
            DuplicateAttribute(ident.name, otherIdent.loc, ident.loc).toFailure
        }
      }

      @@(attributesVal) flatMap {
        case attributes =>
          // the last attribute of a lattice definition must have a lattice interpretation.
          attributes.last.interp match {
            case WeededAst.Interpretation.Set =>
              IllegalNonLatticeAttribute(attributes.last.ident.loc).toFailure
            case WeededAst.Interpretation.Lattice =>

              val index = attributes.indexWhere(_.interp == WeededAst.Interpretation.Lattice)
              val (keys, values) = attributes.splitAt(index)

              // ensure that no non-lattice attribute occurs after `index`.
              values.find(_.interp == WeededAst.Interpretation.Set) match {
                case None => WeededAst.Table.Lattice(past.ident, keys, values, past.loc).toSuccess
                case Some(attr) => IllegalMixedAttributes(values.head.ident.loc, attr.ident.loc).toFailure
              }
          }
      }
    }

    /**
      * Compiles the given parsed index definition `past` to a weeded index definition.
      */
    def compile(past: ParsedAst.Definition.Index): Validation[WeededAst.Definition.Index, WeederError] = {
      if (past.indexes.isEmpty)
        return MissingIndex(past.loc).toFailure

      if (past.indexes.exists(_.isEmpty)) {
        return IllegalIndex(past.loc).toFailure
      }

      WeededAst.Definition.Index(past.ident, past.indexes, past.loc).toSuccess
    }

  }

  object Literals {
    /**
      * Compiles the parsed literal `past` to a weeded literal.
      */
    // TODO: check bounds etc
    def compile(past: ParsedAst.Literal): Validation[WeededAst.Literal, WeederError] = past match {
      case plit: ParsedAst.Literal.Unit => WeededAst.Literal.Unit(plit.loc).toSuccess
      case plit: ParsedAst.Literal.Bool => plit.lit match {
        case "true" => WeededAst.Literal.Bool(lit = true, plit.loc).toSuccess
        case "false" => WeededAst.Literal.Bool(lit = false, plit.loc).toSuccess
        case _ => throw Compiler.InternalCompilerError("Impossible non-boolean value.")
      }
      case plit: ParsedAst.Literal.Char => WeededAst.Literal.Char(plit.lit(0), plit.loc).toSuccess

      case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
        val s = if (sign) s"-$before.$after" else s"$before.$after"
        WeededAst.Literal.Float32(s.toFloat, past.loc).toSuccess
      case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
        val s = if (sign) s"-$before.$after" else s"$before.$after"
        WeededAst.Literal.Float64(s.toDouble, past.loc).toSuccess

      case plit: ParsedAst.Literal.Int8 =>
        val s = if (plit.sign) "-" + plit.lit else plit.lit
        WeededAst.Literal.Int8(s.toByte, plit.loc).toSuccess
      case plit: ParsedAst.Literal.Int16 =>
        val s = if (plit.sign) "-" + plit.lit else plit.lit
        WeededAst.Literal.Int16(s.toShort, plit.loc).toSuccess
      case plit: ParsedAst.Literal.Int32 =>
        val s = if (plit.sign) "-" + plit.lit else plit.lit
        WeededAst.Literal.Int32(s.toInt, plit.loc).toSuccess
      case plit: ParsedAst.Literal.Int64 =>
        val s = if (plit.sign) "-" + plit.lit else plit.lit
        WeededAst.Literal.Int64(s.toLong, plit.loc).toSuccess
      case plit: ParsedAst.Literal.Str => WeededAst.Literal.Str(plit.lit, plit.loc).toSuccess
    }
  }

  object Expression {
    /**
      * Compiles the parsed expression `past` to a weeded expression.
      */
    def compile(past: ParsedAst.Expression): Validation[WeededAst.Expression, WeederError] = past match {
      case exp: ParsedAst.Expression.Lit =>
        Literals.compile(exp.lit) map {
          case lit => WeededAst.Expression.Lit(lit, exp.loc)
        }

      case exp: ParsedAst.Expression.Var =>
        WeededAst.Expression.Var(exp.name, exp.loc).toSuccess

      case exp: ParsedAst.Expression.Apply =>
        @@(compile(exp.lambda), @@(exp.actuals map compile)) map {
          case (lambda, args) => WeededAst.Expression.Apply(lambda, args, exp.loc)
        }

      case exp: ParsedAst.Expression.Lambda =>
        val args = exp.formals map {
          case ParsedAst.FormalArg(ident, annotations, tpe) => WeededAst.FormalArg(ident, tpe)
        }
        compile(exp.body) map {
          case body => WeededAst.Expression.Lambda(Ast.Annotations(List.empty), args.toList, body, exp.tpe, exp.loc)
        }

      case exp: ParsedAst.Expression.Unary => compile(exp.e) map {
        case e => WeededAst.Expression.Unary(exp.op, e, exp.loc)
      }

      case exp: ParsedAst.Expression.Binary =>
        @@(compile(exp.e1), compile(exp.e2)) map {
          case (e1, e2) => WeededAst.Expression.Binary(exp.op, e1, e2, exp.loc)
        }

      case exp: ParsedAst.Expression.ExtendedBinary =>
        @@(compile(exp.e1), compile(exp.e2)) map {
          case (e1, e2) =>
            exp.op match {
              case ExtendedBinaryOperator.Leq =>
                val ident = Name.Ident(exp.leftMostSourcePosition, "⊑", exp.sp2)
                val namespace = Name.NName(exp.leftMostSourcePosition, List.empty, exp.sp2)
                val name = Name.QName(exp.leftMostSourcePosition, namespace, ident, exp.sp2)
                val lambda = WeededAst.Expression.Var(name, exp.loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), exp.loc)

              case ExtendedBinaryOperator.Lub =>
                val ident = Name.Ident(exp.leftMostSourcePosition, "⊔", exp.sp2)
                val namespace = Name.NName(exp.leftMostSourcePosition, List.empty, exp.sp2)
                val name = Name.QName(exp.leftMostSourcePosition, namespace, ident, exp.sp2)
                val lambda = WeededAst.Expression.Var(name, exp.loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), exp.loc)

              case ExtendedBinaryOperator.Glb =>
                val ident = Name.Ident(exp.leftMostSourcePosition, "⊓", exp.sp2)
                val namespace = Name.NName(exp.leftMostSourcePosition, List.empty, exp.sp2)
                val name = Name.QName(exp.leftMostSourcePosition, namespace, ident, exp.sp2)
                val lambda = WeededAst.Expression.Var(name, exp.loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), exp.loc)

              case ExtendedBinaryOperator.Widen =>
                val ident = Name.Ident(exp.leftMostSourcePosition, "▽", exp.sp2)
                val namespace = Name.NName(exp.leftMostSourcePosition, List.empty, exp.sp2)
                val name = Name.QName(exp.leftMostSourcePosition, namespace, ident, exp.sp2)
                val lambda = WeededAst.Expression.Var(name, exp.loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), exp.loc)

              case ExtendedBinaryOperator.Narrow =>
                val ident = Name.Ident(exp.leftMostSourcePosition, "△", exp.sp2)
                val namespace = Name.NName(exp.leftMostSourcePosition, List.empty, exp.sp2)
                val name = Name.QName(exp.leftMostSourcePosition, namespace, ident, exp.sp2)
                val lambda = WeededAst.Expression.Var(name, exp.loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), exp.loc)
            }
        }

      case exp: ParsedAst.Expression.LetMatch =>
        // Compiles a let-match to either a regular let-binding or a pattern match.
        @@(Patterns.compile(exp.pat), compile(exp.value), compile(exp.body)) map {
          case (WeededAst.Pattern.Var(ident, loc), value, body) =>
            WeededAst.Expression.Let(ident, value, body, exp.loc)
          case (pattern, value, body) =>
            val rules = List(pattern -> body)
            WeededAst.Expression.Match(value, rules, exp.loc)
        }

      case exp: ParsedAst.Expression.IfThenElse =>
        @@(compile(exp.e1), compile(exp.e2), compile(exp.e3)) map {
          case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, exp.loc)
        }

      case exp: ParsedAst.Expression.Switch =>
        val rulesVal = exp.rules map {
          case (cond, body) => @@(Expression.compile(cond), Expression.compile(body))
        }
        @@(rulesVal) map {
          case rules => WeededAst.Expression.Switch(rules, exp.loc)
        }

      case exp: ParsedAst.Expression.Match =>
        val rulesVal = exp.rules map {
          case (pat, body) => @@(Patterns.compile(pat), compile(body))
        }
        @@(compile(exp.e), @@(rulesVal)) map {
          case (e, rs) => WeededAst.Expression.Match(e, rs, exp.loc)
        }

      case exp: ParsedAst.Expression.Infix =>
        @@(compile(exp.e1), compile(exp.e2)) map {
          case (e1, e2) => WeededAst.Expression.Apply(WeededAst.Expression.Var(exp.name, exp.loc), List(e1, e2), exp.loc)
        }

      case exp: ParsedAst.Expression.Tag => compile(exp.e) map {
        case e => WeededAst.Expression.Tag(exp.enum, exp.tag, e, exp.loc)
      }

      case exp: ParsedAst.Expression.Tuple =>
        @@(exp.elms map compile) map {
          case elms => WeededAst.Expression.Tuple(elms, exp.loc)
        }

      case ParsedAst.Expression.FNone(sp1, sp2) => ???

      case ParsedAst.Expression.FSome(sp1, elm, sp2) => ???

      case ParsedAst.Expression.FNil(sp1, sp2) => ???

      case ParsedAst.Expression.FList(hd, tl, sp2) => ???

      case exp: ParsedAst.Expression.FSet =>
        @@(exp.elms map compile) map {
          case elms => WeededAst.Expression.Set(elms, exp.loc)
        }

      case ParsedAst.Expression.FMap(sp1, elms, sp2) => ???

      case exp: ParsedAst.Expression.Ascribe =>
        compile(exp.e) map {
          case e => WeededAst.Expression.Ascribe(e, exp.tpe, exp.loc)
        }

      case exp: ParsedAst.Expression.UserError =>
        WeededAst.Expression.Error(exp.tpe, exp.loc).toSuccess

      case exp: ParsedAst.Expression.Bot =>
        val ident = Name.Ident(exp.sp1, "⊥", exp.sp2)
        val namespace = Name.NName(exp.sp1, List.empty, exp.sp2)
        val name = Name.QName(exp.sp1, namespace, ident, exp.sp2)
        val lambda = WeededAst.Expression.Var(name, exp.loc)
        WeededAst.Expression.Apply(lambda, List(), exp.loc).toSuccess

      case exp: ParsedAst.Expression.Top =>
        val ident = Name.Ident(exp.sp1, "⊤", exp.sp2)
        val namespace = Name.NName(exp.sp1, List.empty, exp.sp2)
        val name = Name.QName(exp.sp1, namespace, ident, exp.sp2)
        val lambda = WeededAst.Expression.Var(name, exp.loc)
        WeededAst.Expression.Apply(lambda, List(), exp.loc).toSuccess

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

        case ParsedAst.Pattern.Tag(sp1, enum, tag, ppat, sp2) => visit(ppat) map {
          case pat => WeededAst.Pattern.Tag(enum, tag, pat, mkSL(sp1, sp2))
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
      def compile(past: ParsedAst.Predicate, aliases: Map[String, ParsedAst.Predicate.Alias] = Map.empty): Validation[WeededAst.Predicate.Head, WeederError] = past match {
        case p: ParsedAst.Predicate.Ambiguous =>
          @@(p.terms.map(t => Term.Head.compile(t, aliases))) map {
            case terms => WeededAst.Predicate.Head.Relation(p.name, terms, p.loc)
          }

        case p: ParsedAst.Predicate.Alias => IllegalHeadPredicate(p.loc).toFailure
        case p: ParsedAst.Predicate.Loop => IllegalHeadPredicate(p.loc).toFailure
        case p: ParsedAst.Predicate.NotEqual => IllegalHeadPredicate(p.loc).toFailure
      }

    }

    object Body {

      /**
        * Compiles the given parsed predicate `p` to a weeded body predicate.
        */
      def compile(past: ParsedAst.Predicate): Validation[WeededAst.Predicate.Body, WeederError] = past match {
        case p: ParsedAst.Predicate.Ambiguous =>
          @@(p.terms.map(Term.Body.compile)) map {
            case terms => WeededAst.Predicate.Body.Ambiguous(p.name, terms, past.loc)
          }

        case p: ParsedAst.Predicate.NotEqual =>
          WeededAst.Predicate.Body.NotEqual(p.ident1, p.ident2, p.loc).toSuccess

        case p: ParsedAst.Predicate.Loop => Term.Head.compile(p.term, Map.empty) map {
          case term => WeededAst.Predicate.Body.Loop(p.ident, term, p.loc)
        }

        case p: ParsedAst.Predicate.Alias => throw Compiler.InternalCompilerError("Alias predicate should already have been eliminated.")
      }
    }

  }

  object Term {

    object Head {

      /**
        * Compiles the given parsed head term `past` to a weeded term.
        */
      def compile(past: ParsedAst.Term, aliases: Map[String, ParsedAst.Predicate.Alias]): Validation[WeededAst.Term.Head, WeederError] = past match {
        case term: ParsedAst.Term.Wildcard => IllegalHeadTerm("Wildcards may not occur in head predicates.", term.loc).toFailure
        case term: ParsedAst.Term.Var => aliases.get(term.ident.name) match {
          case None => WeededAst.Term.Head.Var(term.ident, term.loc).toSuccess
          case Some(alias) => compile(alias.term, aliases)
        }
        case term: ParsedAst.Term.Lit => Literals.compile(term.lit) map {
          case lit => WeededAst.Term.Head.Lit(lit, term.loc)
        }
        case term: ParsedAst.Term.Tag => term.t match {
          case None =>
            val unit = WeededAst.Term.Head.Lit(WeededAst.Literal.Unit(term.loc), term.loc)
            WeededAst.Term.Head.Tag(term.enumName, term.tagName, unit, term.loc).toSuccess
          case Some(t) => compile(t, aliases) map {
            case inner => WeededAst.Term.Head.Tag(term.enumName, term.tagName, inner, term.loc)
          }
        }
        case term: ParsedAst.Term.Tuple => term.elms.toList match {
          case Nil => WeededAst.Term.Head.Lit(WeededAst.Literal.Unit(term.loc), term.loc).toSuccess
          case x :: Nil => compile(x, aliases)
          case xs => @@(xs map (x => compile(x, aliases))) map {
            case elms => WeededAst.Term.Head.Tuple(elms, term.loc)
          }
        }
        case term: ParsedAst.Term.Apply =>
          @@(term.args map (t => compile(t, aliases))) map {
            case args => WeededAst.Term.Head.Apply(term.name, args, term.loc)
          }
      }
    }

    object Body {
      /**
        * Compiles the given parsed body term `past` to a weeded term.
        */
      def compile(past: ParsedAst.Term): Validation[WeededAst.Term.Body, WeederError] = past match {
        case term: ParsedAst.Term.Wildcard => WeededAst.Term.Body.Wildcard(term.loc).toSuccess
        case term: ParsedAst.Term.Var => WeededAst.Term.Body.Var(term.ident, term.loc).toSuccess
        case term: ParsedAst.Term.Lit => Literals.compile(term.lit) map {
          case lit => WeededAst.Term.Body.Lit(lit, term.loc)
        }
        case term: ParsedAst.Term.Tag => IllegalBodyTerm("Tags calls may not occur in body predicates.", term.loc).toFailure
        case term: ParsedAst.Term.Tuple => IllegalBodyTerm("Tuples may not occur in body predicates.", term.loc).toFailure
        case term: ParsedAst.Term.Apply => IllegalBodyTerm("Function calls may not occur in body predicates.", term.loc).toFailure
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
            DuplicateAnnotation(x.name, otherAnn.loc, x.loc).toFailure
        }
      }
      @@(result) map Ast.Annotations
    }

    /**
      * Weeds the given parsed annotation `past`.
      */
    def compile(past: ParsedAst.Annotation): Validation[Ast.Annotation, WeederError] = past.name match {
      case "associative" => Ast.Annotation.Associative(past.loc).toSuccess
      case "commutative" => Ast.Annotation.Commutative(past.loc).toSuccess
      case "monotone" => Ast.Annotation.Monotone(past.loc).toSuccess
      case "strict" => Ast.Annotation.Strict(past.loc).toSuccess
      case "unchecked" => Ast.Annotation.Unchecked(past.loc).toSuccess
      case "unsafe" => Ast.Annotation.Unsafe(past.loc).toSuccess
      case _ => IllegalAnnotation(past.name, past.loc).toFailure
    }
  }

  /**
    * Alias for SourceLocation.mk
    */
  private def mkSL(sp1: SourcePosition, sp2: SourcePosition): SourceLocation = SourceLocation.mk(sp1, sp2)
}
