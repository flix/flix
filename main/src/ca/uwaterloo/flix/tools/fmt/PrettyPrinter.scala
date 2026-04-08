package ca.uwaterloo.flix.tools.fmt

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Tree, TreeKind}
import ca.uwaterloo.flix.tools.fmt.Doc.{empty, line, nest, pretty, space, text}

object PrettyPrinter {

  def format(tree: Tree): String = {
    val doc = traverse(tree)
    Doc.pretty(doc)
  }

  private def traverse(tree: Tree): Doc = tree.kind match {
    case TreeKind.Root                          => prettyRoot(tree)
    case TreeKind.AnnotationList                => prettyAnnotationList(tree)
    case TreeKind.ModifierList                  => prettyModifierList(tree)
    case TreeKind.Ident                         => prettyIdent(tree)
    case TreeKind.Operator                      => prettyOperator(tree)
    case TreeKind.QName                         => prettyQName(tree)
    case TreeKind.Parameter                     => prettyParameter(tree)
    case TreeKind.ParameterList                 => prettyParameterList(tree)
    case TreeKind.ArgumentList                  => prettyArgumentList(tree)
    case TreeKind.Case                          => prettyCase(tree)
    case TreeKind.Decl.Def                      => prettyDef(tree)
    case TreeKind.Decl.Redef                    => prettyDef(tree)
    case TreeKind.Decl.Signature                => prettyDef(tree)
    case TreeKind.Decl.Op                       => prettyDef(tree)
    case TreeKind.Decl.Law                      => prettyDef(tree)
    case TreeKind.Decl.Module                   => prettyModule(tree)
    case TreeKind.Decl.Enum                     => prettyEnum(tree)
    case TreeKind.Decl.Instance                 => prettyInstance(tree)
    case TreeKind.Decl.Effect                   => prettyEffect(tree)
    case TreeKind.Decl.TypeAlias                => prettyTypeAlias(tree)
    case TreeKind.Decl.AssociatedTypeDef        => prettyTypeAlias(tree)
    case TreeKind.Decl.AssociatedTypeSig        => prettyTypeAlias(tree)
    case TreeKind.Decl.Trait                    => prettyTrait(tree)
    case TreeKind.Decl.Struct                   => prettyStruct(tree)
    case TreeKind.Decl.RestrictableEnum         => prettyEnum(tree)
    case TreeKind.Expr.Binary                   => prettyBinary(tree)
    case TreeKind.Expr.Apply                    => prettyApply(tree)
    case TreeKind.Expr.Lambda                   => prettyLambda(tree)
    case TreeKind.Expr.Block                    => prettyBlock(tree)
    case TreeKind.Expr.Statement                => prettyStatement(tree)
    case TreeKind.Expr.LetMatch                 => prettyLetMatch(tree)
    case TreeKind.Expr.NewObject                => prettyNewObject(tree)
    case TreeKind.Expr.InvokeConstructor        => prettyInvokeConstructor(tree)
    case TreeKind.Expr.JvmMethod                => prettyDef(tree)
    case TreeKind.Expr.Match                    => prettyMatch(tree)
    case TreeKind.Expr.ExtMatch                 => prettyMatch(tree)
    case TreeKind.Expr.Select                   => prettySelect(tree)
    case TreeKind.Expr.Foreach                  => prettyForeach(tree)
    case TreeKind.Expr.FixpointConstraintSet    => prettyFixpointConstraintSet(tree)
    case TreeKind.Expr.LiteralVector            => prettyCommaBracket(tree)
    case TreeKind.Expr.LiteralList              => prettyCommaBracket(tree)
    case TreeKind.Expr.LiteralSet               => prettyCommaBracket(tree)
    case TreeKind.Expr.LiteralMap               => prettyCommaBracket(tree)
    case TreeKind.Expr.LiteralArray             => prettyCommaBracket(tree)
    case TreeKind.Expr.RecordOperation          => prettyCommaBracket(tree)
    case TreeKind.Expr.ParYield                 => prettyParYield(tree)
    case TreeKind.Expr.LocalDef                 => prettyDef(tree)
    case TreeKind.Type.Binary                   => prettyBinary(tree)
    case TreeKind.Type.Schema                   => prettyCommaBracket(tree)
    case TreeKind.Type.Extensible               => prettyCommaBracket(tree)
    case TreeKind.Type.Record                   => prettyCommaBracket(tree)
    case TreeKind.UsesOrImports.UseOrImportList => prettyUseOrImportList(tree)
    case TreeKind.UsesOrImports.Import          => prettyImport(tree)
    case TreeKind.UsesOrImports.Use             => prettyUse(tree)
    case TreeKind.Expr.RestrictableChoose       => prettyRestrictableChoose(tree)
    case TreeKind.Expr.RestrictableChooseStar   => prettyRestrictableChoose(tree)
    case TreeKind.Expr.ForMonadic               => prettyFor(tree)
    case TreeKind.Expr.ForApplicative           => prettyFor(tree)
    case _ => prettyFallback(tree)
  }

  private def prettyParYield(tree: Tree): Doc = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val openIdx = children.indexWhere {
      case token: Token if token.kind == TokenKind.ParenL => true
      case _ => false
    }
    val closeIdx = children.lastIndexWhere {
      case token: Token if token.kind == TokenKind.ParenR => true
      case _ => false
    }

    if (openIdx < 0 || closeIdx < 0) return prettyFallback(tree)

    val header = children.slice(0, openIdx)
    val body = children.slice(openIdx + 1, closeIdx)
    val tail = children.slice(closeIdx + 1, children.length)

    val headerDoc = header.map(prettyChild)
      .reduceLeftOption(_ <+> _)
      .getOrElse(empty)

    val bodyDoc = body.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Semi =>
        acc <> text(";") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }

    val tailDoc = tail.map(prettyChild)
      .reduceLeftOption(_ <+> _)
      .getOrElse(empty)

    val tailPart = if (tail.isEmpty) empty else nest(4, line <> tailDoc)

    localLayout(tree) {
      headerDoc <+> text("(") <> bodyDoc <> text(")") <> tailPart
    }
  }

  private def prettyRestrictableChoose(tree: Tree): Doc =
    prettyBracket(tree,
      headerJoin = cs => spaceJoin(cs, Set.empty))

  private def prettyFor(tree: Tree): Doc = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val openIdx = children.indexWhere {
      case token: Token if token.kind == TokenKind.ParenL => true
      case _ => false
    }
    val closeIdx = children.lastIndexWhere {
      case token: Token if token.kind == TokenKind.ParenR => true
      case _ => false
    }

    if (openIdx < 0 || closeIdx < 0) return prettyFallback(tree)

    val header = children.slice(0, openIdx)
    val body = children.slice(openIdx + 1, closeIdx)
    val tail = children.slice(closeIdx + 1, children.length)

    val headerDoc = header.map(prettyChild)
      .reduceLeftOption(_ <+> _)
      .getOrElse(empty)

    val bodyDoc = body.zipWithIndex.foldLeft((empty, Option.empty[SyntaxTree.Child])) {
      case ((acc, _), (token: Token, _)) if token.kind == TokenKind.Semi =>
        (acc <> text(";") <> line, Some(token))
      case ((acc, prev), (child, _)) =>
        val gap = (prev.flatMap(rightMostToken), leftMostToken(child)) match {
          case (Some(r), Some(l)) => getGap(r, l)
          case _ => empty
        }
        (acc <> gap <> prettyChild(child), Some(child))
    }._1

    val tailDoc = tail.map(prettyChild)
      .reduceLeftOption(_ <+> _)
      .getOrElse(empty)

    val tailPart = if (tail.isEmpty) empty else space <> tailDoc

    localLayout(tree) {
      headerDoc <+> text("(") <> nest(4, line <> bodyDoc) <> line <> text(")") <> tailPart
    }
  }

  private val bracketPairs: List[(TokenKind, TokenKind, String, String)] = List(
    (TokenKind.HashCurlyL, TokenKind.CurlyR,    "#{", "}"),
    (TokenKind.CurlyL,     TokenKind.CurlyR,    "{",  "}"),
    (TokenKind.ParenL,     TokenKind.ParenR,     "(",  ")"),
    (TokenKind.HashParenL, TokenKind.ParenR,     "#(", ")"),
    (TokenKind.BracketL,   TokenKind.BracketR,   "[",  "]"),
  )

  private def extractDocAndAnnotations(tree: Tree): (Doc, Doc, Boolean, Boolean, Array[SyntaxTree.Child]) = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }
    val (docChildren, rest1) = children.partition {
      case t: Tree if t.kind == TreeKind.Doc => true
      case _ => false
    }
    val (annChildren, rest) = rest1.partition {
      case t: Tree if t.kind == TreeKind.AnnotationList => true
      case _ => false
    }
    val docDoc = docChildren.map(prettyChild).reduceLeftOption(_ <|> _).getOrElse(empty)
    val annDoc = annChildren.map(prettyChild).reduceLeftOption(_ <|> _).getOrElse(empty)
    (docDoc, annDoc, docChildren.nonEmpty, annChildren.nonEmpty, rest)
  }

  private def wrapWithDocAndAnn(tree: Tree, inner: Array[SyntaxTree.Child] => Doc): Doc = {
    val (docDoc, annDoc, hasDoc, hasAnn, rest) = extractDocAndAnnotations(tree)
    prepend(docDoc, hasDoc, prepend(annDoc, hasAnn, inner(rest)))
  }

  /**
    * Formatting for declarations with brackets (e.g enum, instance, trait struct, module).
    * Handles doc and annotations, finds the bracket pair, and delegates to prettyBracket.
    * @param tree the declaration tree
    * @param headerJoin function to join header children (before the opening bracket)
    * @param bodySep separator between body elements (default: hard line)
    * @param bodyJoin optional function to join body children (if None, defaults to joining with bodySep)
    * @param fallback fallback formatter if no brackets are found (default: prettyFallback)
    * @param filterBody whether to filter out empty children from the body (default: true)
    * @return the formatted declaration as Doc
    */
  private def prettyDeclBracket(
    tree: Tree,
    headerJoin: Array[SyntaxTree.Child] => Doc = defaultHeaderJoin,
    bodySep: Doc = line,
    bodyJoin: Option[Array[SyntaxTree.Child] => Doc] = None,
    fallback: Tree => Doc = prettyFallback,
    filterBody: Boolean = true
  ): Doc = wrapWithDocAndAnn(tree, rest => {
    val stripped = tree.copy(children = rest)
    prettyBracket(stripped, headerJoin, bodySep, bodyJoin, fallback, filterBody)
  })

  /**
    * General formatting for constructs with brackets. Finds the first matching bracket pair and splits children into header and body.
    *
    * @param tree the tree to format
    * @param headerJoin function to join header children (before the opening bracket)
    * @param bodySep separator between body elements (default: hard line)
    * @param bodyJoin optional function to join body children (if None, defaults to joining with bodySep)
    * @param fallback fallback formatter if no brackets are found (default: prettyFallback)
    * @param filterBody whether to filter out empty children from the body (default: true)
    * @return the formatted construct as Doc
    */
  private def prettyBracket(
    tree: Tree,
    headerJoin: Array[SyntaxTree.Child] => Doc = defaultHeaderJoin,
    bodySep: Doc = line,
    bodyJoin: Option[Array[SyntaxTree.Child] => Doc] = None,
    fallback: Tree => Doc = prettyFallback,
    filterBody: Boolean = true
  ): Doc = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val bracket = bracketPairs.flatMap {
      case (openKind, closeKind, openText, closeText) =>
        val oi = children.indexWhere {
          case token: Token if token.kind == openKind => true
          case _ => false
        }
        val ci = children.lastIndexWhere {
          case token: Token if token.kind == closeKind => true
          case _ => false
        }
        if (oi >= 0 && ci > oi) Some((oi, ci, openText, closeText))
        else None
    }.headOption

    bracket match {
      case None => fallback(tree)
      case Some((openIndex, closeIndex, openText, closeText)) =>
        val header = children.slice(0, openIndex)
        val body   = children.slice(openIndex + 1, closeIndex)

        val headerDoc = headerJoin(header)

        val bodyParts = if (filterBody) body.filter {
          case t: Tree if t.children.isEmpty => false
          case _ => true
        } else body

        val bodyDoc = bodyJoin match {
          case Some(join) => join(bodyParts)
          case None =>
            bodyParts
              .map(prettyChild)
              .reduceLeftOption(_ <> bodySep <> _)
              .getOrElse(empty)
        }

        val noGap = header.lastOption.exists {
          case token: Token => token.text.endsWith("#")
          case _ => false
        }

        val openDoc =
          if (header.isEmpty) text(openText)
          else if (noGap) headerDoc <> text(openText)
          else headerDoc <+> text(openText)

        localLayout(tree) {
          if (bodyParts.isEmpty) openDoc <> text(closeText)
          else openDoc <> nest(4, line <> bodyDoc) <> line <> text(closeText)
        }
    }
  }

  /**
    * Formatting for constructs with comma-separated bodies (e.g literal vectors, lists, sets, maps, arrays).
    * Finds the first matching bracket pair and joins body children with commas and bars, without filtering out empty children.
    *
    * @param tree the tree to format
    * @return the formatted construct as Doc
    */
  private def prettyCommaBracket(tree: Tree): Doc =
    prettyBracket(tree,
      bodyJoin = Some(commaBodyJoin),
      filterBody = false)

  private def commaBodyJoin(children: Array[SyntaxTree.Child]): Doc = {
    if (children.isEmpty) return empty
    children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Comma =>
        acc <> text(",") <> line
      case (acc, token: Token) if token.kind == TokenKind.Bar =>
        acc <> space <> text("|") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formatting for enum and restrictable enum declarations.
    * Finds the first matching bracket pair and joins body children with hard lines, without filtering out empty children.
    *
    * @param tree the enum declaration tree
    * @return the formatted enum declaration as Doc
    */
  private def prettyEnum(tree: Tree): Doc =
    prettyDeclBracket(tree,
      headerJoin = declHeaderJoin,
      bodyJoin = Some(enumBodyJoin),
      filterBody = false)

  /**
    * Formatting for instance declarations.
    *
    * @param tree the instance declaration tree
    * @return the formatted instance declaration as Doc
    */
  private def prettyInstance(tree: Tree): Doc =
    prettyDeclBracket(tree,
      headerJoin = cs => spaceJoin(cs,
        noSpacePairs = Set((TreeKind.Ident, TreeKind.TypeParameterList))))

  /**
    * Formatting for effect declarations.
    *
    * @param tree the effect declaration tree
    * @return the formatted effect declaration as Doc
    */
  private def prettyEffect(tree: Tree): Doc =
    prettyDeclBracket(tree,
      headerJoin = cs => spaceJoin(cs, Set.empty))

  /**
    * Formatting for trait declarations.
    *
    * @param tree the trait declaration tree
    * @return the formatted trait declaration as Doc
    */
  private def prettyTrait(tree: Tree): Doc =
    prettyDeclBracket(tree, headerJoin = declHeaderJoin)

  /**
    * Formatting for struct declarations.
    *
    * @param tree the struct declaration tree
    * @return the formatted struct declaration as Doc
    */
  private def prettyStruct(tree: Tree): Doc =
    prettyDeclBracket(tree, headerJoin = declHeaderJoin, filterBody = false)

  /**
    * Formatting for match and select expressions.
    *
    * @param tree the match or select expression tree
    * @return the formatted match or select expression as Doc
    */
  private def prettyMatch(tree: Tree): Doc =
    prettyBracket(tree,
      headerJoin = cs => spaceJoin(cs, Set.empty))

  /**
    * Formatting for select expressions.
    *
    * @param tree the select expression tree
    * @return the formatted select expression as Doc
    */
  private def prettySelect(tree: Tree): Doc =
    prettyBracket(tree,
      headerJoin = cs => spaceJoin(cs, Set.empty))

  /**
    * Formatting for fixpoint constraint sets.
    *
    * @param tree the fixpoint constraint set tree
    * @return the formatted fixpoint constraint set as Doc
    */
  private def prettyFixpointConstraintSet(tree: Tree): Doc =
    prettyBracket(tree)

  /**
    * Formatting for declaration headers.
    * Joins children with spaces, suppressing space between an identifier and a type parameter list, and before colons.
    *
    * @param cs the header children to join
    * @return the formatted header as Doc
    */
  private def declHeaderJoin(cs: Array[SyntaxTree.Child]): Doc =
    spaceJoin(cs,
      noSpacePairs = Set((TreeKind.Ident, TreeKind.TypeParameterList)),
      noSpaceBefore = Set(TokenKind.Colon))

  /**
    * Formatting for enum bodies.
    * Joins children with hard lines, filtering out commas, empty trees, and empty tokens.
    * This allows for more flexible formatting of enum cases, including optional trailing commas and blank lines.
    *
    * @param children the body children to join
    * @return
    */
  private def enumBodyJoin(children: Array[SyntaxTree.Child]): Doc = {
    children.filter {
        case token: Token if token.kind == TokenKind.Comma => false
        case t: Tree if t.children.isEmpty => false
        case _ => true
      }.map(prettyChild)
      .reduceLeftOption(_ <> line <> _)
      .getOrElse(empty)
  }

  /**
    * Formatting for case expressions.
    *
    * @param tree the case expression tree
    * @return the formatted case expression as Doc
    */
  private def prettyCase(tree: Tree): Doc = {
    val (commentChildren, parts) = tree.children.filter {
      case token: Token if token.kind == TokenKind.Comma => false
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }.partition {
      case t: Tree if t.kind == TreeKind.Doc => true
      case _ => false
    }

    val commentDoc = commentChildren.map(prettyChild)
      .reduceLeftOption(_ <|> _)
      .getOrElse(empty)
    val hasComment = commentChildren.nonEmpty

    val hasCase = parts.exists {
      case token: Token if token.kind == TokenKind.KeywordCase => true
      case _ => false
    }

    val arrowIndex = parts.indexWhere {
      case token: Token if token.kind == TokenKind.ArrowThickR => true
      case _ => false
    }

    val caseDoc = if (arrowIndex < 0) {
      val inner = parts.map(prettyChild)
        .reduceLeftOption(_ <+> _)
        .getOrElse(empty)
      if (hasCase) inner else text("case") <+> inner
    } else {
      val header = parts.take(arrowIndex + 1)
      val body = parts.drop(arrowIndex + 1)

      val headerDoc = {
        val h = header.map(prettyChild).reduceLeftOption(_ <+> _).getOrElse(empty)
        if (hasCase) h else text("case") <+> h
      }

      if (body.isEmpty) headerDoc
      else {
        val bodyDoc = body.map(prettyChild)
          .reduceLeftOption(_ <|> _)
          .getOrElse(empty)
        localLayout(tree) {
          headerDoc <> nest(4, line <> bodyDoc)
        }
      }
    }

    prepend(commentDoc, hasComment, caseDoc)
  }

  /**
    * Formatting for module declarations.
    *
    * @param tree the module declaration tree
    * @return the formatted module declaration as Doc
    */
  private def prettyModule(tree: Tree): Doc =
    prettyDeclBracket(tree, bodySep = line <> line)

  /**
    * Formatting for definitions.
    *
    * @param tree the definition tree
    * @return the formatted definition as Doc
    */
  private def prettyDef(tree: Tree): Doc = {
    val (docDoc, annDoc, hasDoc, hasAnn, rest) = extractDocAndAnnotations(tree)

    val eqIndex = rest.indexWhere {
      case token: Token if token.kind == TokenKind.Equal => true
      case _ => false
    }

    if (eqIndex < 0) {
      val sig = spaceJoin(rest,
        noSpacePairs = Set((TreeKind.Ident, TreeKind.ParameterList)),
        noSpaceBefore = Set(TokenKind.Colon))
      return prepend(docDoc, hasDoc, prepend(annDoc, hasAnn, sig))
    }

    val sigParts  = rest.take(eqIndex)
    val bodyParts = rest.drop(eqIndex + 1)

    val sig = spaceJoin(sigParts,
      noSpacePairs = Set((TreeKind.Ident, TreeKind.ParameterList)),
      noSpaceBefore = Set(TokenKind.Colon))

    val body = bodyParts.map(prettyChild)
      .reduceLeftOption(_ <> _)
      .getOrElse(empty)

    val bodyIsBlock = bodyParts.exists {
      case t: Tree =>
        t.kind == TreeKind.Expr.Block ||
          t.children.exists {
            case inner: Tree => inner.kind == TreeKind.Expr.Block
            case _ => false
          } ||
          leftMostToken(t).exists(tok => bracketPairs.exists(_._1 == tok.kind))
      case _ => false
    }

    val defDoc = localLayout(tree) {
      if (bodyIsBlock) sig <+> text("=") <+> body
      else sig <+> text("=") <> nest(4, line <> body)
    }

    prepend(docDoc, hasDoc, prepend(annDoc, hasAnn, defDoc))
  }

  /**
    * Formatting for type alias.
    *
    * @param tree the type alias tree
    * @return the formatted type alias as Doc
    */
  private def prettyTypeAlias(tree: Tree): Doc = wrapWithDocAndAnn(tree, rest => {
    spaceJoin(rest, noSpacePairs = Set.empty)
  })

  /**
    * Formatting for binary expressions and types.
    *
    * @param tree the binary expression or type tree
    * @return the formatted binary expression or type as Doc
    */
  private def prettyBinary(tree: Tree): Doc = {
    val parts = tree.children.map(prettyChild)
    if (parts.length == 3) {
      val isPipe = tree.children(1) match {
        case t: Tree => leftMostToken(t).exists(_.text == "|>")
        case tok: Token => tok.text == "|>"
      }
      localLayout(tree) {
        if (isPipe) parts(0) <> nest(4, line <> parts(1) <+> parts(2))
        else parts(0) <> line <> parts(1) <+> parts(2)
      }
    } else {
      parts.reduceLeftOption(_ <+> _).getOrElse(empty)
    }
  }

  /**
    * Formatting for blocks.
    *
    * @param tree the block expression tree
    * @return the formatted block expression as Doc
    */
  private def prettyBlock(tree: Tree): Doc =
    prettyBracket(tree)

  /**
    * Formatting for statements.
    *
    * @param tree the statement tree
    * @return the formatted statement as Doc
    */
  private def prettyStatement(tree: Tree): Doc = localLayout(tree) {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Semi =>
        acc <> text(";") <> line
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formatting for let-match expressions.
    *
    * @param tree the let-match expression tree
    * @return the formatted let-match expression as Doc
    */
  private def prettyLetMatch(tree: Tree): Doc = localLayout(tree) {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.KeywordLet =>
        acc <> text("let") <> space
      case (acc, token: Token) if token.kind == TokenKind.Equal =>
        acc <> space <> text("=") <> space
      case (acc, token: Token) if token.kind == TokenKind.Semi =>
        acc <> text(";") <> line
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formatting for foreach expressions.
    * Finds the first closing parenthesis to split the header and body.
    *
    * @param tree the foreach expression tree
    * @return the formatted foreach expression as Doc
    */
  private def prettyForeach(tree: Tree): Doc = {
    val children = tree.children

    val closeParenIdx = children.indexWhere {
      case token: Token if token.kind == TokenKind.ParenR => true
      case _ => false
    }

    if (closeParenIdx < 0) return prettyFallback(tree)

    val head = children.slice(0, closeParenIdx + 1)
    val body = children.slice(closeParenIdx + 1, children.length)

    val headDoc = head.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.KeywordForeach =>
        acc <> text("foreach") <> space
      case (acc, token: Token) if token.kind == TokenKind.Semi =>
        acc <> text(";") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }

    val bodyDoc = body.map(prettyChild)
      .reduceLeftOption(_ <> _)
      .getOrElse(empty)

    localLayout(tree) {
      headDoc <> nest(4, line <> bodyDoc)
    }
  }

  /**
    * Formatting for lambda expressions.
    * Joins children with spaces, replacing arrow tokens with "->" and surrounding them with spaces.
    *
    * @param tree the lambda expression tree
    * @return the formatted lambda expression as Doc
    */
  private def prettyLambda(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.ArrowThinRWhitespace =>
        acc <> space <> text("->") <> space
      case (acc, token: Token) if token.kind == TokenKind.ArrowThinRTight =>
        acc <> space <> text("->") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formatting for new object expressions.
    *
    * @param tree the new object expression tree
    * @return the formatted new object expression as Doc
    */
  private def prettyNewObject(tree: Tree): Doc = {
    val children = tree.children
    val openIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.CurlyL => true
      case _ => false
    }
    val closeIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.CurlyR => true
      case _ => false
    }

    if (openIndex < 0 || closeIndex < 0) return prettyFallback(tree)

    val before = children.slice(0, openIndex)
    val body = children.slice(openIndex + 1, closeIndex)
    val after = children.slice(closeIndex + 1, children.length)

    val prefix = before.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.KeywordNew =>
        acc <> text("new") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }

    val inner = body.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
    val suffix = after.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

    localLayout(tree) {
      prefix <> space <> text("{") <>
        nest(4, line <> inner) <>
        line <> text("}") <> suffix
    }
  }

  /**
    * Formatting for apply expressions.
    * Joins children with spaces, replacing "new" keyword with "new " and surrounding it with spaces.
    *
    * @param tree the apply expression tree
    * @return the formatted apply expression as Doc
    */
  private def prettyApply(tree: Tree): Doc =
    keywordSpaced(tree, TokenKind.KeywordNew, "new")

  /**
    * Formatting for invoke constructor expressions.
    * Joins children with spaces, replacing "new" keyword with "new " and surrounding it with spaces.
    *
    * @param tree the invoke constructor expression tree
    * @return the formatted invoke constructor expression as Doc
    */
  private def prettyInvokeConstructor(tree: Tree): Doc =
    keywordSpaced(tree, TokenKind.KeywordNew, "new")

  /**
    * Formatting for import declarations.
    * Joins children with spaces, replacing "import" keyword with "import " and surrounding it with spaces.
    *
    * @param tree the import declaration tree
    * @return the formatted import declaration as Doc
    */
  private def prettyImport(tree: Tree): Doc =
    keywordSpaced(tree, TokenKind.KeywordImport, "import")

  private def prettyUse(tree: Tree): Doc =
    keywordSpaced(tree, TokenKind.KeywordUse, "use")

  /**
    * Helper for formatting constructs that start with a specific keyword.
    * Joins children with spaces, replacing the specified keyword token with the given keyword string and surrounding it with spaces.
    *
    * @param tree the tree to format
    * @param kind the token kind of the keyword to replace
    * @param kw the keyword string to use in the output
    * @return the formatted construct as Doc
    */
  private def keywordSpaced(tree: Tree, kind: TokenKind, kw: String): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == kind =>
        acc <> text(kw) <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  private def prettyIdent(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyOperator(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyQName(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyModifierList(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <+> _).getOrElse(empty)

  private def prettyAnnotationList(tree: Tree): Doc = {
    val anns = tree.children.collect { case token: Token => text(token.text) }
    anns.reduceLeftOption(_ <|> _).getOrElse(empty)
  }

  /**
    * Formatting for parameter lists.
    * Joins children with spaces, replacing commas with ", " and surrounding them with spaces.
    *
    * @param tree the parameter list tree
    * @return the formatted parameter list as Doc
    */
  private def prettyParameterList(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Comma =>
        acc <> text(",") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formatting for individual parameters.
    * Joins children with spaces, replacing colons with ": " and surrounding them with spaces
    *
    * @param tree the parameter tree
    * @return the formatted parameter as Doc
    */
  private def prettyParameter(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Colon =>
        acc <> text(":") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formatting for argument lists.
    * Joins children with spaces, replacing commas with ", " and surrounding them with spaces.
    *
    * @param tree the argument list tree
    * @return the formatted argument list as Doc
    */
  private def prettyArgumentList(tree: Tree): Doc = {
    val hasArgs = tree.children.exists {
      case t: Tree => t.kind == TreeKind.Argument || t.kind == TreeKind.ArgumentNamed
      case _ => false
    }
    if (!hasArgs) return text("(") <> text(")")
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Comma =>
        acc <> text(",") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formatting for the root of the syntax tree. Joins non-empty children with hard lines.
    *
    * @param tree the root tree
    * @return the formatted root as Doc
    */
  private def prettyRoot(tree: Tree): Doc = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }
    if (children.isEmpty) return empty
    children.map(prettyChild).reduceLeft(_ <|> empty <|> _)
  }

  /**
    * Formatting for use and import lists. Joins non-empty children with hard lines.
    *
    * @param tree the use or import list tree
    * @return the formatted use or import list as Doc
    */
  private def prettyUseOrImportList(tree: Tree): Doc = {
    val children = tree.children.collect { case t: Tree => traverse(t) }
    if (children.isEmpty) return empty
    children.reduceLeft(_ <|> _)
  }

  private def prettyChild(child: SyntaxTree.Child): Doc = child match {
    case token: Token => text(token.text)
    case tree: Tree   => traverse(tree)
  }

  private def prettyFallback(tree: Tree): Doc = {
    val children = tree.children
    if (children.isEmpty) return empty

    children.sliding(2).foldLeft(prettyChild(children.head)) {
      case (acc, Array(prev, next)) =>
        val gap = (rightMostToken(prev), leftMostToken(next)) match {
          case (Some(r), Some(l)) => getGap(r, l)
          case _                  => empty
        }
        acc <> gap <> prettyChild(next)
      case (acc, _) => acc
    }
  }

  private def getGap(prev: Token, next: Token): Doc = {
    if (prev.endIndex >= next.startIndex) empty
    else {
      val between = prev.src.data.slice(prev.endIndex, next.startIndex).mkString
      if (between.contains('\n')) line
      else if (between.nonEmpty) space
      else empty
    }
  }

  private def defaultHeaderJoin(cs: Array[SyntaxTree.Child]): Doc =
    cs.map(prettyChild).reduceLeftOption(_ <+> _).getOrElse(empty)

  /**
    * Joins children with spaces, suppressing space between specified pairs of tree kinds and before specified token kinds.
    *
    * @param children the children to join
    * @param noSpacePairs set of pairs of tree kinds between which no space should be added
    * @param noSpaceBefore set of token kinds before which no space should be added
    * @return the formatted children as Doc
    */
  private def spaceJoin(
    children: Array[SyntaxTree.Child],
    noSpacePairs: Set[(TreeKind, TreeKind)],
    noSpaceBefore: Set[TokenKind] = Set.empty
  ): Doc = {
    if (children.isEmpty) return empty

    children.sliding(2).foldLeft(prettyChild(children.head)) {
      case (acc, Array(prev, next)) =>
        val noSpace = (prev, next) match {
          case (p: Tree, n: Tree) if noSpacePairs.contains((p.kind, n.kind)) => true
          case (_, token: Token) if noSpaceBefore.contains(token.kind)       => true
          case _ => false
        }
        val sep = if (noSpace) empty else space
        acc <> sep <> prettyChild(next)
      case (acc, _) => acc
    }
  }

  /**
    * Prepends a prefix to the body if the prefix is present, otherwise returns the body.
    * For example is used with annotations and doc comments.
    *
    * @param prefix the prefix to prepend if present
    * @param hasPrefix whether the prefix is present (TODO: Remove flag)
    * @param body the main body to return if the prefix is not present, or to append to the prefix if it is present
    * @return the combined prefix and body if the prefix is present, otherwise just the body
    */
  private def prepend(prefix: Doc, hasPrefix: Boolean, body: Doc): Doc =
    if (hasPrefix) prefix <|> body else body

  private def leftMostToken(child: SyntaxTree.Child): Option[Token] = child match {
    case token: Token => Some(token)
    case tree: Tree   => tree.children.collectFirst(Function.unlift(c => leftMostToken(c)))
  }

  private def rightMostToken(child: SyntaxTree.Child): Option[Token] = child match {
    case token: Token => Some(token)
    case tree: Tree   => tree.children.reverse.collectFirst(Function.unlift(c => rightMostToken(c)))
  }

  /**
    * Determines the layout of the given tree based on whether it is is a singleline or multiline construct.
    *
    * @param tree the tree to determine the layout for
    * @return Layout.SingleLine if the tree is single-line, Layout.MultiLine otherwise
    */
  private def layoutOf(tree: Tree): Layout =
    if (tree.loc.isSingleLine) Layout.SingleLine else Layout.MultiLine

  /**
    * Sets the layout of the given document based on the layout of the given tree.
    *
    * @param tree the tree to determine the layout from
    * @param doc the document to set the layout for
    * @return the document with the layout set according to the tree's layout
    */
  private def localLayout(tree: Tree)(doc: Doc): Doc =
    Doc.setLayout(layoutOf(tree), doc)
}
