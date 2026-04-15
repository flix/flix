package ca.uwaterloo.flix.tools.fmt

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Tree, TreeKind}
import ca.uwaterloo.flix.tools.fmt.Doc.{empty, hardStack, hardline, line, nest, pretty, space, text}

object PrettyPrinter {

  def format(tree: Tree): String = {
    val doc = traverse(tree)
    Doc.pretty(doc)
  }

  private def traverse(tree: Tree): Doc = tree.kind match {
    case TreeKind.Root                          => prettyRoot(tree)
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
    case TreeKind.Expr.LambdaMatch              => prettyLambda(tree)
    case TreeKind.Expr.LambdaExtMatch           => prettyLambda(tree)
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
    case TreeKind.Expr.MatchRuleFragment        => prettyMatchRuleFragment(tree)
    case TreeKind.Expr.IfThenElse               => prettyIfThenElse(tree)
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

    // Comma-bracket constructs: open, comma-separated body, close
    case TreeKind.Expr.Tuple                    => prettyCommaBracket(tree)
    case TreeKind.Expr.NewStruct                => prettyCommaBracket(tree)
    case TreeKind.Type.Tuple                    => prettyCommaBracket(tree)
    case TreeKind.Type.ArgumentList             => prettyCommaBracket(tree)
    case TreeKind.Type.EffectSet               => prettyCommaBracket(tree)
    case TreeKind.Type.ConstraintList           => prettyCommaBracket(tree)
    case TreeKind.Type.RecordRow                => prettyCommaBracket(tree)
    case TreeKind.Type.RecordFieldFragment      => prettyFallback(tree) // field: Type (no brackets)
    case TreeKind.Type.SchemaRow                => prettyCommaBracket(tree)
    case TreeKind.Type.CaseSet                  => prettyCommaBracket(tree)
    case TreeKind.Pattern.Tuple                 => prettyCommaBracket(tree)
    case TreeKind.Pattern.Record                => prettyCommaBracket(tree)
    case TreeKind.Predicate.ParamList           => prettyCommaBracket(tree)
    case TreeKind.Predicate.PatternList         => prettyCommaBracket(tree)
    case TreeKind.Predicate.TermList            => prettyCommaBracket(tree)
    case TreeKind.Decl.EqualityConstraintList   => prettyCommaBracket(tree)
    case TreeKind.UsesOrImports.ImportMany      => prettyCommaBracket(tree)
    case TreeKind.UsesOrImports.UseMany         => prettyCommaBracket(tree)
    case TreeKind.TypeParameterList             => prettyCommaBracket(tree)
    case TreeKind.Expr.StructGet                => prettyStructGet(tree)
    case TreeKind.Expr.StructPut                => prettyStructPut(tree)
    case TreeKind.Expr.Unary                    => prettyUnary(tree)
    case TreeKind.Pattern.Unary                 => prettyUnary(tree)
    case _ => prettyFallback(tree)
  }

  /**
    * Formatting for struct field updates (e.g. `obj->field = value`).
    *
    * @param tree the struct field update expression tree
    * @return the formatted struct field update expression as Doc
    */
  private def prettyStructPut(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)

    if (children.length != 5) return prettyFallback(tree)

    val lhsObj   = prettyChild(children(0))
    val field    = prettyChild(children(2))
    val rhsValue = prettyChild(children(4))

    localLayout(tree) {
      lhsObj <> text("->") <> field <> text(" = ") <> rhsValue
    }
  }

  /**
    * Formatting for struct field access (e.g. `obj->field`).
    *
    * @param tree the struct field access expression tree
    * @return the formatted struct field access expression as Doc
    */
  private def prettyStructGet(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    if (children.length != 3) return prettyFallback(tree)

    val lhs = prettyChild(children(0))
    val rhs = prettyChild(children(2))

    localLayout(tree) {
      lhs <> text("->") <> rhs
    }
  }

  /**
    * Formatting for if-then-else expressions.
    * Delegates to prettyFallback which uses joinChildrenWithComments.
    *
    * TODO: Define explicit single-line and multi-line formats for if else construct.
    */
  private def prettyIfThenElse(tree: Tree): Doc = prettyFallback(tree)

  private def prettyMatchRuleFragment(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    val arrowIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.ArrowThickR => true
      case _ => false
    }
    if (arrowIndex < 0) return prettyFallback(tree)

    val header = children.take(arrowIndex + 1)
    val body = children.drop(arrowIndex + 1)

    val headerDoc = header.map(prettyChild)
      .reduceLeftOption(_ <+> _)
      .getOrElse(empty)

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

  private def prettyParYield(tree: Tree): Doc =
    splitAtBracket(filterEmpty(tree.children)) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, tail)) =>
        val headerDoc = defaultHeaderJoin(header)
        val bodyDoc = joinChildren(body,
          TokenKind.Semi -> (text(";") <> space))
        val tailDoc = defaultHeaderJoin(tail)
        val tailPart = if (tail.isEmpty) empty else nest(4, line <> tailDoc)
        localLayout(tree) {
          headerDoc <+> text(open) <> bodyDoc <> text(close) <> tailPart
        }
    }

  private def prettyRestrictableChoose(tree: Tree): Doc =
    splitAtBracket(filterEmpty(tree.children)) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, _)) =>
        val bodyParts = filterEmpty(body)
        val promoted = promoteLeadingComments(bodyParts)
        val bodyDoc = joinChildrenWithComments(promoted, codeGap = (_, _) => line)
        bracketDoc(tree, header, open, bodyDoc, close, bodyParts.isEmpty,
          headerJoin = cs => spaceJoin(cs, Set.empty))
    }

  private def prettyFor(tree: Tree): Doc =
    splitAtBracket(filterEmpty(tree.children)) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, tail)) =>
        val headerDoc = defaultHeaderJoin(header)
        val bodyDoc = joinChildrenWithComments(body)

        val tailDoc = defaultHeaderJoin(tail)
        localLayout(tree) {
          val tailStart = tail.headOption.flatMap(leftMostToken)
          val isBlock = tailStart.exists(t => t.kind == TokenKind.CurlyL || t.kind == TokenKind.ParenL)
          headerDoc <+> text(open) <> nest(4, line <> bodyDoc) <> line <> text(close) <>
            (if (isBlock) space <> tailDoc else if (tail.nonEmpty) space <> tailDoc else empty)
        }
    }

  private val bracketPairs: List[(TokenKind, TokenKind, String, String)] = List(
    (TokenKind.HashCurlyL, TokenKind.CurlyR,    "#{", "}"),
    (TokenKind.CurlyL,     TokenKind.CurlyR,    "{",  "}"),
    (TokenKind.ParenL,     TokenKind.ParenR,     "(",  ")"),
    (TokenKind.HashParenL, TokenKind.ParenR,     "#(", ")"),
    (TokenKind.BracketL,   TokenKind.BracketR,   "[",  "]"),
  )

  /**
    * Type of splitting an array of children at a matching bracket pair.
    *
    * @param header children before the opening bracket
    * @param open   the opening bracket text
    * @param body   children between the brackets
    * @param close  the closing bracket text
    * @param tail   children after the closing bracket
    */
  private case class BracketSplit(
    header: Array[SyntaxTree.Child],
    open: String,
    body: Array[SyntaxTree.Child],
    close: String,
    tail: Array[SyntaxTree.Child]
  )

  /**
    * Filters out empty [[Children]].
    *
    * @param children the array of children to filter
    * @return a new array containing only non-empty children
    */
  private def filterEmpty(children: Array[SyntaxTree.Child]): Array[SyntaxTree.Child] =
    children.filter {
      case t: Tree if t.children.isEmpty && !isCommentChild(t) => false
      case _ => true
    }

  /**
    * Finds the first matching bracket pair in the children and splits them
    * into header, body, and tail. Returns None if no brackets are found.
    *
    * @param children the array of children to search for brackets
    * @return Some(BracketSplit) if a matching bracket pair is found, None otherwise
    */
  private def splitAtBracket(children: Array[SyntaxTree.Child]): Option[BracketSplit] = {
    bracketPairs.flatMap {
      case (openKind, closeKind, openText, closeText) =>
        val oi = children.indexWhere {
          case token: Token if token.kind == openKind => true
          case _ => false
        }
        val ci = children.lastIndexWhere {
          case token: Token if token.kind == closeKind => true
          case _ => false
        }
        if (oi >= 0 && ci > oi) Some(BracketSplit(
          header = children.slice(0, oi),
          open = openText,
          body = children.slice(oi + 1, ci),
          close = closeText,
          tail = children.slice(ci + 1, children.length)
        ))
        else None
    }.headOption
  }

  /**
    * The common bracket formatting pattern shared by most bracket-based formatters.
    *
    * @param tree       the tree used to determine layout
    * @param header     the children before the opening bracket
    * @param open       the opening bracket text
    * @param bodyDoc    the already-formatted body document
    * @param close      the closing bracket text
    * @param bodyIsEmpty whether the body is empty
    * @param headerJoin function to join header children
    * @return the formatted bracket construct
    */
  private def bracketDoc(
    tree: Tree,
    header: Array[SyntaxTree.Child],
    open: String,
    bodyDoc: Doc,
    close: String,
    bodyIsEmpty: Boolean,
    headerJoin: Array[SyntaxTree.Child] => Doc = defaultHeaderJoin
  ): Doc = {
    val noGap = header.lastOption.exists {
      case token: Token => token.text.endsWith("#")
      case _ => false
    }
    val openDoc =
      if (header.isEmpty) text(open)
      else if (noGap) headerJoin(header) <> text(open)
      else headerJoin(header) <+> text(open)

    val pad = Doc.layoutChoice(empty, line)
    openDoc <> nest(4, pad <> bodyDoc) <> line <> text(close)
  }

  private def extractDocAndAnnotations(tree: Tree): (Doc, Doc, Boolean, Boolean, Array[SyntaxTree.Child]) = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty && !isCommentChild(t) => false
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
    val docs = docChildren.map(prettyChild).toList
    val docDoc =
      if (docs.isEmpty) empty
      else hardStack(docs)
    val annDoc = hardStack(annChildren.map(prettyChild).toList)
    (docDoc, annDoc, docChildren.nonEmpty, annChildren.nonEmpty, rest)
  }

  private def wrapWithDocAndAnn(tree: Tree, inner: Array[SyntaxTree.Child] => Doc): Doc = {
    val (docDoc, annDoc, hasDoc, hasAnn,   rest) = extractDocAndAnnotations(tree)
    prepend(docDoc, hasDoc, prepend(annDoc, hasAnn, inner(rest)))
  }

  /**
    * Formatting for declarations with brackets (e.g enum, instance, trait struct, module).
    * Handles doc and annotations, finds the bracket pair, and delegates to prettyBracket.
    *
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
      case t: Tree if t.children.isEmpty && !isCommentChild(t) => false
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
        val tail   = children.slice(closeIndex + 1, children.length)

        val headerDoc = headerJoin(header)

        val bodyParts = if (filterBody) body.filter {
          case t: Tree if t.children.isEmpty && !isCommentChild(t) => false
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

        val tailDoc = if (tail.isEmpty) empty
        else space <> joinChildrenWithComments(tail)

        val pad = Doc.layoutChoice(empty, line)
        localLayout(tree) {
          if (bodyParts.isEmpty) openDoc <> text(closeText) <> tailDoc
          else openDoc <> nest(4, pad <> bodyDoc) <> line <> text(closeText) <> tailDoc
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

  private def commaBodyJoin(children: Array[SyntaxTree.Child]): Doc =
    joinChildren(children,
      TokenKind.Comma -> (text(",") <> line),
      TokenKind.Bar   -> (space <> text("|") <> space))

  /**
    * Formatting for enum and restrictable enum declarations.
    * Finds the first matching bracket pair and joins body children with hard lines, without filtering out empty children.
    *
    * @param tree the enum declaration tree
    * @return the formatted enum declaration as Doc
    */
  private def prettyEnum(tree: Tree): Doc =
    wrapWithDocAndAnn(tree, rest =>
      splitAtBracket(filterEmpty(rest)) match {
        case Some(bs) =>
          val bodyChildren = filterEmpty(bs.body)

          val bodyDoc =
            if (bodyChildren.isEmpty) empty
            else hardStack(bodyChildren.map(prettyChild).toList)

          bracketDoc(
            tree = tree,
            header = bs.header,
            open = bs.open,
            bodyDoc = bodyDoc,
            close = bs.close,
            bodyIsEmpty = bodyChildren.isEmpty,
            headerJoin = declHeaderJoin
          )

        case None => prettyFallback(tree)
      }
    )

  private def prettyCase(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    if (children.isEmpty) return prettyFallback(tree)
    joinChildrenWithComments(children, codeGap = (_, _) => space)
  }

  /**
    * Extracts leading line comments from inside child trees and promotes
    * them to the parent level.
    *
    * This is a workaround for the fact that the parser currently attaches a trailing comment
    * as a leading child of the next TreeKind, which causes it to be formatted as a leading comment.
    * TODO: Discuss different approaches and maybe we can change it in the parser?
    */
  private def promoteLeadingComments(children: Array[SyntaxTree.Child]): Array[SyntaxTree.Child] = {
    import scala.collection.mutable.ArrayBuffer
    val result = ArrayBuffer[SyntaxTree.Child]()

    for (child <- children) {
      child match {
        case tree: Tree if tree.children.nonEmpty =>
          val leading = ArrayBuffer[SyntaxTree.Child]()
          val rest = ArrayBuffer[SyntaxTree.Child]()
          var foundCode = false

          for (c <- tree.children) {
            if (!foundCode && isPromotableComment(c)) {
              leading += c
            } else if (!foundCode && isSkippableLeading(c)) {
            } else {
              foundCode = true
              rest += c
            }
          }

          result ++= leading
          if (leading.isEmpty) result += child
          else if (rest.nonEmpty) result += tree.copy(children = rest.toArray)

        case _ =>
          result += child
      }
    }

    result.toArray
  }

  private def isPromotableComment(child: SyntaxTree.Child): Boolean = child match {
    case token: Token => token.kind == TokenKind.CommentLine
    case tree: Tree =>
      isCommentChild(tree) &&
        leftMostToken(tree).exists(t => t.kind == TokenKind.CommentLine || t.kind == TokenKind.CommentBlock)
    case _ => false
  }

  private def isSkippableLeading(child: SyntaxTree.Child): Boolean = child match {
    case token: Token if token.kind == TokenKind.Comma => true
    case tree: Tree if tree.children.isEmpty && !isCommentChild(tree) => true
    case tree: Tree if tree.children.isEmpty && isCommentChild(tree) => true
    case _ => false
  }

  private def prettyInstance(tree: Tree): Doc = wrapWithDocAndAnn(tree, rest =>
    splitAtBracket(rest) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, _)) =>
        val bodyParts = filterEmpty(body)
        val promoted = promoteLeadingComments(bodyParts)
        val bodyDoc = joinChildrenWithComments(promoted, codeGap = (_, _) => line)
        bracketDoc(tree, header, open, bodyDoc, close, bodyParts.isEmpty,
          headerJoin = cs => spaceJoin(cs,
            noSpacePairs = Set((TreeKind.Ident, TreeKind.TypeParameterList))))
    })

  private def prettyEffect(tree: Tree): Doc = wrapWithDocAndAnn(tree, rest =>
    splitAtBracket(rest) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, _)) =>
        val bodyParts = filterEmpty(body)
        val promoted = promoteLeadingComments(bodyParts)
        val bodyDoc = joinChildrenWithComments(promoted, codeGap = (_, _) => line)
        bracketDoc(tree, header, open, bodyDoc, close, bodyParts.isEmpty,
          headerJoin = cs => spaceJoin(cs, Set.empty))
    })

  private def prettyTrait(tree: Tree): Doc = wrapWithDocAndAnn(tree, rest =>
    splitAtBracket(rest) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, _)) =>
        val bodyParts = filterEmpty(body)
        val promoted = promoteLeadingComments(bodyParts)
        val bodyDoc = joinChildrenWithComments(promoted, codeGap = (_, _) => line)
        bracketDoc(tree, header, open, bodyDoc, close, bodyParts.isEmpty,
          headerJoin = declHeaderJoin)
    })

  private def prettyStruct(tree: Tree): Doc = wrapWithDocAndAnn(tree, rest =>
    splitAtBracket(rest) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, _)) =>
        val bodyDoc = commaBodyJoin(body)
        bracketDoc(tree, header, open, bodyDoc, close, body.isEmpty,
          headerJoin = declHeaderJoin)
    })

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
    * This tries to join children with appropriate spacing, while keeping comments preserved.
    *
    * Again very much a workaround for comments to be properly formatted.
    */
  private def joinChildrenWithComments(
    children: Array[SyntaxTree.Child],
    codeGap: (SyntaxTree.Child, SyntaxTree.Child) => Doc = structuralGap
  ): Doc = {
    if (children.isEmpty) return empty

    val ordered = reorderComments(children)

    var result = empty
    var lastCodeChild: Option[SyntaxTree.Child] = None
    var afterLineComment = false
    var first = true

    for (child <- ordered) {
      if (isCommentChild(child)) {
        val isTrailing = !afterLineComment && lastCodeChild.exists(prev =>
          rightMostToken(prev).exists(p =>
            leftMostToken(child).exists(n =>
              p.end.lineOneIndexed == n.start.lineOneIndexed)))

        if (isTrailing) {
          result = result <> space <> prettyChild(child)
        } else {
          if (first) result = prettyChild(child)
          else result = result <> hardline <> prettyChild(child)
        }

        afterLineComment = leftMostToken(child).exists(t =>
          t.kind == TokenKind.CommentLine || t.kind == TokenKind.CommentDoc)
        first = false
      } else {
        val gap = if (afterLineComment) hardline
        else if (first) empty
        else lastCodeChild.map(prev => codeGap(prev, child)).getOrElse(hardline)

        result = result <> gap <> prettyChild(child)
        lastCodeChild = Some(child)
        afterLineComment = false
        first = false
      }
    }
    result
  }

  /**
    * Reorders children so that comments appear after all code children
    * on the same source line.
    */
  private def reorderComments(children: Array[SyntaxTree.Child]): Array[SyntaxTree.Child] = {
    import scala.collection.mutable.ArrayBuffer
    val result = ArrayBuffer[SyntaxTree.Child]()
    val commentBuf = ArrayBuffer[SyntaxTree.Child]()
    var lastCodeEndLine: Option[Int] = None

    for (child <- children) {
      if (isCommentChild(child)) {
        commentBuf += child
      } else {
        for (c <- commentBuf) {
          val commentLine = leftMostToken(c).map(_.start.lineOneIndexed)
          val isTrailingOnPrev = lastCodeEndLine.exists(prevEnd =>
            commentLine.exists(_ == prevEnd))

          if (isTrailingOnPrev) {
            result += c
          } else {
            result += c
          }
        }
        commentBuf.clear()

        result += child
        lastCodeEndLine = rightMostToken(child).map(_.end.lineOneIndexed)
      }
    }

    result ++= commentBuf
    result.toArray
  }

  private def isCommentChild(child: SyntaxTree.Child): Boolean = child match {
    case token: Token => isCommentKind(token.kind)
    case tree: Tree   => tree.children.nonEmpty &&
      (tree.kind == TreeKind.CommentList || tree.kind == TreeKind.Doc)
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
    * Formatting for unary expressions and patterns.
    * Symbolic operators (-x, !x, ~x) are tight-joined.
    * Keyword operators (not x, lazy x) are space-joined.
    *
    * @param tree the unary expression or pattern tree
    * @return the formatted unary expression or pattern as Doc
    */
  private def prettyUnary(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    if (children.isEmpty) return empty
    val isKeywordOp = leftMostToken(children.head).exists(_.kind.isKeyword)
    if (isKeywordOp)
      children.map(prettyChild).reduceLeftOption(_ <+> _).getOrElse(empty)
    else
      children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
  }

  /**
    * Formatting for binary expressions and types.
    *
    * @param tree the binary expression or type tree
    * @return the formatted binary expression or type as Doc
    */
  private def prettyBinary(tree: Tree): Doc = {
    val parts = tree.children.map(prettyChild)
    if (parts.length == 3) {
      val endsWithClose = rightMostToken(tree.children(0)).exists(t =>
        t.kind == TokenKind.CurlyR
      )
      localLayout(tree) {
        if (endsWithClose)
          parts(0) <+> parts(1) <+> parts(2)
        else
          parts(0) <> line <> parts(1) <+> parts(2)
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
    joinChildren(tree.children,
      TokenKind.Semi -> (text(";") <> line))
  }

  /**
    * Formatting for let-match expressions.
    *
    * @param tree the let-match expression tree
    * @return the formatted let-match expression as Doc
    */
  private def prettyLetMatch(tree: Tree): Doc = localLayout(tree) {
    joinChildren(tree.children,
      TokenKind.KeywordLet -> (text("let") <> space),
      TokenKind.Equal      -> (space <> text("=") <> space),
      TokenKind.Semi       -> (text(";") <> line))
  }

  /**
    * Formatting for foreach expressions.
    * Finds the first closing parenthesis to split the header and body.
    *
    * @param tree the foreach expression tree
    * @return the formatted foreach expression as Doc
    */
  private def prettyForeach(tree: Tree): Doc =
    splitAtBracket(tree.children) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, tail)) =>
        val headerDoc = joinChildren(header,
          TokenKind.KeywordForeach -> (text("foreach") <> space))
        val bodyDoc = joinChildren(body,
          TokenKind.Semi -> (text(";") <> space))
        val tailDoc = tail.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
        val tailStart = tail.headOption.flatMap(leftMostToken)
        val isBlock = tailStart.exists(t => t.kind == TokenKind.CurlyL || t.kind == TokenKind.ParenL)
        localLayout(tree) {
          headerDoc <+> text(open) <> bodyDoc <> text(close) <>
            (if (isBlock) space <> tailDoc else nest(4, line <> tailDoc))
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
    val children = filterEmpty(tree.children)
    val arrowIndex = children.indexWhere {
      case token: Token => token.kind == TokenKind.ArrowThinRWhitespace ||
        token.kind == TokenKind.ArrowThinRTight
      case _ => false
    }
    if (arrowIndex < 0) return prettyFallback(tree)

    val header = children.take(arrowIndex)
    val body = children.drop(arrowIndex + 1)

    val headerDoc = header.map(prettyChild)
      .reduceLeftOption(_ <+> _).getOrElse(empty)
    val bodyDoc = body.map(prettyChild)
      .reduceLeftOption(_ <|> _).getOrElse(empty)

    localLayout(tree) {
      headerDoc <+> text("->") <> nest(4, line <> bodyDoc)
    }
  }

  /**
    * Formatting for new object expressions.
    *
    * @param tree the new object expression tree
    * @return the formatted new object expression as Doc
    */
  private def prettyNewObject(tree: Tree): Doc =
    splitAtBracket(tree.children) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, tail)) =>
        val prefix = joinChildren(header,
          TokenKind.KeywordNew -> (text("new") <> space))
        val inner = body.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
        val suffix = tail.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
        localLayout(tree) {
          prefix <+> text(open) <>
            nest(4, line <> inner) <>
            line <> text(close) <> suffix
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
  private def keywordSpaced(tree: Tree, kind: TokenKind, kw: String): Doc =
    joinChildren(tree.children, kind -> (text(kw) <> space))

  private def prettyIdent(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyOperator(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyQName(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyModifierList(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <+> _).getOrElse(empty)

  /**
    * Formatting for parameter lists.
    * Joins children with spaces, replacing commas with ", " and surrounding them with spaces.
    *
    * @param tree the parameter list tree
    * @return the formatted parameter list as Doc
    */
  private def prettyParameterList(tree: Tree): Doc =
    joinChildren(tree.children,
      TokenKind.Comma -> (text(",") <> space))

  /**
    * Formatting for individual parameters.
    * Joins children with spaces, replacing colons with ": " and surrounding them with spaces
    *
    * @param tree the parameter tree
    * @return the formatted parameter as Doc
    */
  private def prettyParameter(tree: Tree): Doc =
    joinChildren(tree.children,
      TokenKind.Colon -> (text(":") <> space))

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

    splitAtBracket(filterEmpty(tree.children)) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, _)) =>
        val bodyDoc = commaBodyJoin(body)
        val openDoc =
          if (header.isEmpty) text(open)
          else defaultHeaderJoin(header) <> text(open)
        localLayout(tree) {
          openDoc <> Doc.align(bodyDoc) <> Doc.layoutChoice(empty, line) <> text(close)
        }
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
      case t: Tree if t.children.isEmpty && !isCommentChild(t) => false
      case _ => true
    }
    if (children.isEmpty) return empty
    children.map(prettyChild).reduceLeft((a, b) => a <> hardline <> hardline <> b)
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
    hardStack(children.toList)
  }

  private def prettyChild(child: SyntaxTree.Child): Doc = child match {
    case token: Token => text(token.text)
    case tree: Tree   => traverse(tree)
  }

  /**
    * Walks an array of children, replacing tokens of specific kinds with
    * the given Doc replacements and rendering everything else via [[prettyChild]].
    *
    * @param children     the children to walk
    * @param replacements pairs of (TokenKind, Doc) specifying what to emit for each token kind
    * @return the joined document
    */
  private def joinChildren(
    children: Array[SyntaxTree.Child],
    replacements: (TokenKind, Doc)*
  ): Doc = {
    val replMap = replacements.toMap
    if (children.isEmpty) return empty
    children.foldLeft(empty) {
      case (acc, token: Token) if replMap.contains(token.kind) =>
        acc <> replMap(token.kind)
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * This is the last resort for pretty printing if no other formatting rule applies.
    *
    * @param tree the tree to format
    * @return the formatted tree as Doc.
    */
  private def prettyFallback(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    if (children.isEmpty) return empty
    localLayout(tree) {
      joinChildrenWithComments(children)
    }
  }

  private val TightAfter: Set[TokenKind] = Set(
    TokenKind.ParenL,
    TokenKind.BracketL,
    TokenKind.CurlyL,
    TokenKind.HashParenL,
    TokenKind.HashCurlyL,
    TokenKind.Dot,
    TokenKind.Hash,
    TokenKind.ListHash,
    TokenKind.SetHash,
    TokenKind.MapHash,
    TokenKind.VectorHash,
    TokenKind.ArrayHash,
    TokenKind.BarHash
  )

  private val TightBefore: Set[TokenKind] = Set(
    TokenKind.ParenR,
    TokenKind.BracketR,
    TokenKind.CurlyR,
    TokenKind.Dot,
    TokenKind.DotWhiteSpace,
    TokenKind.Comma,
    TokenKind.Semi,
  )

  /**
    * Gap between two children.
    *
    * This tries to preserve idempotency, nonetheless, it might not be perfect for now.
    * We must implement all cases to assure idempotency full.
    */
  private def structuralGap(prev: SyntaxTree.Child, next: SyntaxTree.Child): Doc = {
    val prevKind = rightMostToken(prev).map(_.kind)
    val nextKind = leftMostToken(next).map(_.kind)

    val tight = prevKind.exists(TightAfter.contains) ||
      nextKind.exists(TightBefore.contains)
    if (tight) empty else line
  }

  private def isCommentKind(kind: TokenKind): Boolean =
    kind == TokenKind.CommentLine ||
      kind == TokenKind.CommentDoc ||
      kind == TokenKind.CommentBlock

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
    if (hasPrefix) prefix <> hardline <> body else body

  private def leftMostToken(child: SyntaxTree.Child): Option[Token] = child match {
    case token: Token => Some(token)
    case tree: Tree   => tree.children.collectFirst(Function.unlift(c => leftMostToken(c)))
  }

  private def rightMostToken(child: SyntaxTree.Child): Option[Token] = child match {
    case token: Token => Some(token)
    case tree: Tree   => tree.children.reverse.collectFirst(Function.unlift(c => rightMostToken(c)))
  }

  /**
    * Determines the layout of the given tree based on whether its code
    * content (excluding comments) spans a single line or multiple lines.
    *
    * @param tree the tree to determine the layout for
    * @return Layout.SingleLine if the code spans one line, Layout.MultiLine otherwise
    */
  private def layoutOf(tree: Tree): Layout = {
    val first = leftMostCodeToken(tree)
    val last = rightMostCodeToken(tree)
    (first, last) match {
      case (Some(f), Some(l)) =>
        if (f.start.lineOneIndexed == l.end.lineOneIndexed) Layout.SingleLine
        else Layout.MultiLine
      case _ =>
        if (tree.loc.isSingleLine) Layout.SingleLine else Layout.MultiLine
    }
  }

  private def leftMostCodeToken(child: SyntaxTree.Child): Option[Token] = child match {
    case token: Token if isCommentToken(token) => None
    case token: Token => Some(token)
    case tree: Tree   => tree.children.collectFirst(Function.unlift(c => leftMostCodeToken(c)))
  }

  private def rightMostCodeToken(child: SyntaxTree.Child): Option[Token] = child match {
    case token: Token if isCommentToken(token) => None
    case token: Token => Some(token)
    case tree: Tree   => tree.children.reverse.collectFirst(Function.unlift(c => rightMostCodeToken(c)))
  }

  private def isCommentToken(token: Token): Boolean = isCommentKind(token.kind)

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
