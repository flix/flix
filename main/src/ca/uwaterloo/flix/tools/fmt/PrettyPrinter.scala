package ca.uwaterloo.flix.tools.fmt

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Tree, TreeKind}
import ca.uwaterloo.flix.tools.fmt.Doc.{align, empty, hardStack, hardline, line, nest, pretty, space, text}

object PrettyPrinter {

  def format(tree: Tree): String = {
    val result = Doc.pretty(traverse(tree))
    if (result.endsWith("\n")) result else result + "\n"
  }
  private def traverse(tree: Tree): Doc = {
    if (tree.children.isEmpty) empty else formatTree(tree)
  }

  private def formatTree(tree: Tree): Doc = tree.kind match {
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
    case TreeKind.Expr.Apply                    => prettyNewKeyword(tree)
    case TreeKind.Expr.Lambda                   => prettyLambda(tree)
    case TreeKind.Expr.LambdaMatch              => prettyLambda(tree)
    case TreeKind.Expr.LambdaExtMatch           => prettyLambda(tree)
    case TreeKind.Expr.Block                    => prettyBlock(tree)
    case TreeKind.Expr.Statement                => prettyStatement(tree)
    case TreeKind.Expr.LetMatch                 => prettyLetMatch(tree)
    case TreeKind.Expr.NewObject                => prettyNewObject(tree)
    case TreeKind.Expr.InvokeConstructor        => prettyNewKeyword(tree)
    case TreeKind.Expr.JvmMethod                => prettyDef(tree)
    case TreeKind.Expr.Match                    => prettyMatch(tree)
    case TreeKind.Expr.ExtMatch                 => prettyMatch(tree)
    case TreeKind.Expr.Select                   => prettyMatch(tree)
    case TreeKind.Expr.SelectRuleFragment          => prettyMatchRuleFragment(tree)
    case TreeKind.Expr.SelectRuleDefaultFragment   => prettyMatchRuleFragment(tree)
    case TreeKind.Expr.Foreach                  => prettyForeach(tree)
    case TreeKind.Expr.Try                      => prettyTry(tree)
    case TreeKind.Expr.TryCatchBodyFragment     => prettyMatch(tree)
    case TreeKind.Expr.TryCatchRuleFragment     => prettyTryCatchRuleFragment(tree)
    case TreeKind.Expr.Ascribe                  => prettyAscribe(tree)
    case TreeKind.Expr.Throw                    => keywordSpaced(tree, TokenKind.KeywordThrow, "throw")
    case TreeKind.Expr.StringInterpolation      => prettyStringInterpolation(tree)
    case TreeKind.Expr.Spawn                    => spaceJoin(filterEmpty(tree.children), Set.empty)
    case TreeKind.Expr.OpenVariant              => keywordSpaced(tree, TokenKind.KeywordOpenVariant, "open_variant")
    case TreeKind.Expr.OpenVariantAs            => spaceJoin(filterEmpty(tree.children), Set.empty)
    case TreeKind.Expr.UncheckedCast            => prettyUncheckedCast(tree)
    case TreeKind.Expr.CheckedTypeCast          => prettyCheckedCast(tree, TokenKind.KeywordCheckedCast, "checked_cast")
    case TreeKind.Expr.CheckedEffectCast            => prettyCheckedCast(tree, TokenKind.KeywordCheckedECast, "checked_ecast")
    case TreeKind.Expr.Unsafe                       => prettyUnsafe(tree)
    case TreeKind.Expr.UnsafeAsEffFragment          => keywordSpaced(tree, TokenKind.KeywordAs, "as")
    case TreeKind.Expr.FixpointConstraintSet        => prettyFixpointConstraintSet(tree)
    case TreeKind.Expr.FixpointConstraint           => prettyFixpointConstraint(tree)
    case TreeKind.Expr.FixpointQuery                => prettyFixpointQuery(tree)
    case TreeKind.Expr.FixpointQueryWithProvenance  => prettyFixpointQuery(tree)
    case TreeKind.Expr.FixpointSelect               => prettyFixpointSelect(tree)
    case TreeKind.Expr.FixpointFromFragment         => prettyFixpointFromFragment(tree)
    case TreeKind.Expr.FixpointWhere                => prettyFixpointWhere(tree)
    case TreeKind.Expr.FixpointWith                 => prettyFixpointWith(tree)
    case TreeKind.Expr.FixpointInject               => prettyFixpointInject(tree)
    case TreeKind.Expr.FixpointSolveWithProject     => prettyFixpointSolve(tree)
    case TreeKind.Expr.FixpointSolveWithProvenance  => prettyFixpointSolve(tree)
    case TreeKind.Expr.FixpointLambda               => prettyFixpointLambda(tree)
    case TreeKind.Expr.LiteralVector            => prettyCommaBracket(tree)
    case TreeKind.Expr.LiteralList              => prettyCommaBracket(tree)
    case TreeKind.Expr.LiteralSet               => prettyCommaBracket(tree)
    case TreeKind.Expr.LiteralMap               => prettyCommaBracket(tree)
    case TreeKind.Expr.LiteralArray             => prettyCommaBracket(tree)
    case TreeKind.Expr.RecordOperation          => prettyCommaBracket(tree)
    case TreeKind.Expr.RecordOpExtend           => prettyRecordOpExtend(tree)
    case TreeKind.Expr.RecordOpRestrict         => prettyRecordOpRestrict(tree)
    case TreeKind.Expr.RecordOpUpdate           => prettyRecordFieldAssign(tree)
    case TreeKind.Expr.ParYield                 => prettyParYield(tree)
    case TreeKind.Expr.LocalDef                 => prettyLocalDef(tree)
    case TreeKind.Expr.MatchRuleFragment        => prettyMatchRuleFragment(tree)
    case TreeKind.Expr.ExtMatchRuleFragment     => prettyMatchRuleFragment(tree)
    case TreeKind.Expr.IfThenElse               => prettyIfThenElse(tree)
    case TreeKind.Type.Binary                   => prettyBinary(tree)
    case TreeKind.Type.Apply                    => prettyTypeConcat(tree)
    case TreeKind.Type.Type                     => prettyTypeConcat(tree)
    case TreeKind.Type.Schema                   => prettyCommaBracket(tree)
    case TreeKind.Type.Extensible               => prettyCommaBracket(tree)
    case TreeKind.Type.Record                   => prettySpacedCommaBracket(tree)
    case TreeKind.UsesOrImports.UseOrImportList => prettyUseOrImportList(tree)
    case TreeKind.UsesOrImports.Import          => prettyImport(tree)
    case TreeKind.UsesOrImports.Use             => prettyUse(tree)
    case TreeKind.Expr.RestrictableChoose       => prettyRestrictableChoose(tree)
    case TreeKind.Expr.RestrictableChooseStar   => prettyRestrictableChoose(tree)
    case TreeKind.Expr.ForMonadic               => prettyFor(tree)
    case TreeKind.Expr.ForApplicative           => prettyFor(tree)
    case TreeKind.Expr.ForFragmentGenerator     => prettyForFragment(tree)
    case TreeKind.Expr.ForFragmentLet           => prettyForFragment(tree)
    case TreeKind.Expr.ForFragmentGuard         => prettyForFragment(tree)
    case TreeKind.CaseBody                      => prettyCaseBody(tree)
    case TreeKind.Expr.Tuple                    => prettyCommaBracket(tree)
    case TreeKind.Expr.NewStruct                => prettyCommaBracket(tree)
    case TreeKind.Type.Tuple                    => prettyCommaBracket(tree)
    case TreeKind.Type.ArgumentList             => prettyCommaBracket(tree)
    case TreeKind.Type.EffectSet                => prettySpacedCommaBracket(tree)
    case TreeKind.Type.ConstraintList           => prettyConstraintList(tree)
    case TreeKind.Type.Constraint               => prettyTypeConcat(tree)
    case TreeKind.Type.RecordRow                => prettySpacedCommaBracket(tree)
    case TreeKind.Type.RecordFieldFragment      => prettyRecordFieldAssign(tree)
    case TreeKind.Type.SchemaRow                => prettyCommaBracket(tree)
    case TreeKind.Type.CaseSet                  => prettyCommaBracket(tree)
    case TreeKind.Pattern.Tuple                 => prettyCommaBracket(tree)
    case TreeKind.Pattern.Record                => prettySpacedCommaBracket(tree)
    case TreeKind.Pattern.RecordFieldFragment   => prettyRecordFieldAssign(tree)
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
    case TreeKind.Expr.Region                   => prettyRegion(tree)
    case TreeKind.Expr.Run                      => prettyRun(tree)
    case TreeKind.Expr.RunWithBodyExpr          => prettyRunWith(tree)
    case TreeKind.Expr.RunWithRuleFragment      => prettyDef(tree)
    case TreeKind.Expr.Handler                  => prettyHandler(tree)
    case TreeKind.Expr.LiteralStructFieldFragment => prettyRecordFieldAssign(tree)
    case TreeKind.Expr.LiteralMapKeyValueFragment => prettyLiteralMapKeyValueFragment(tree)
    case TreeKind.Expr.ParYieldFragment         => prettyForFragment(tree)
    case TreeKind.Pattern.Tag                   => prettyPatternTag(tree)
    case TreeKind.Pattern.TagBody               => prettyCommaBracket(tree)
    case TreeKind.Pattern.FCons                 => prettyFCons(tree)
    case TreeKind.StructField                   => prettyStructField(tree)
    case TreeKind.DerivationList                => prettyConstraintList(tree)
    case TreeKind.Type.Unary                    => prettyUnary(tree)
    case TreeKind.Type.Ascribe                  => prettyAscribe(tree)
    case TreeKind.Type.Effect                   => prettyTypeConcat(tree)
    case TreeKind.Expr.Paren                    => prettyCommaBracket(tree)
    case TreeKind.ArgumentNamed                 => prettyRecordFieldAssign(tree)
    case TreeKind.UsesOrImports.Alias           => prettyAlias(tree)
    case TreeKind.Decl.EqualityConstraintFragment => prettyEqualityConstraint(tree)
    case TreeKind.AnnotationList                => spaceJoin(filterEmpty(tree.children), Set.empty)
    case _ => prettyFallback(tree)
  }

  private def prettyEqualityConstraint(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children), TokenKind.Tilde -> (space <> text("~") <> space))

  private def prettyAlias(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children), TokenKind.ArrowThickR -> (space <> text("=>") <> space))

  private def prettyStructField(tree: Tree): Doc =
    spaceJoin(filterEmpty(tree.children), noSpacePairs = Set.empty, noSpaceBefore = Set(TokenKind.Colon))

  private def prettyFCons(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    if (children.length != 2) return prettyFallback(tree)

    val headDoc = children(0) match {
      case t: Tree =>
        val inner = filterEmpty(t.children).filterNot {
          case tok: Token => tok.kind == TokenKind.ColonColon
          case _          => false
        }
        if (inner.isEmpty) prettyChild(children(0))
        else if (inner.length == 1) prettyChild(inner(0))
        else joinWithGap(inner)
      case _ => prettyChild(children(0))
    }
    headDoc <> space <> text("::") <> space <> prettyChild(children(1))
  }

  private def prettyPatternTag(tree: Tree): Doc =
    spaceJoin(filterEmpty(tree.children), noSpacePairs = Set((TreeKind.QName, TreeKind.Pattern.TagBody)))

  private def prettyLiteralMapKeyValueFragment(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children), TokenKind.ArrowThickR -> (space <> text("=>") <> space))

  private def prettyStructPut(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)

    if (children.length != 5) return prettyFallback(tree)

    val lhsObj   = prettyChild(children(0))
    val field    = prettyChild(children(2))
    val rhsValue = prettyChild(children(4))

    localLayout(tree) {
      lhsObj <> text("->") <> field <> space <> text("=") <> space <> rhsValue
    }
  }

  private def prettyStructGet(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    if (children.length != 3) return prettyFallback(tree)

    val lhs = prettyChild(children(0))
    val rhs = prettyChild(children(2))

    localLayout(tree) {
      lhs <> text("->") <> rhs
    }
  }

  private def prettyRegion(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    val blockIdx = children.indexWhere {
      case t: Tree if t.kind == TreeKind.Expr.Block => true
      case _ => false
    }
    if (blockIdx < 0) return spaceJoin(children, Set.empty)
    val header   = children.take(blockIdx)
    val blockDoc = prettyChild(children(blockIdx))
    localLayout(tree) {
      spaceJoin(header, Set.empty) <+> blockDoc
    }
  }

  private def prettyRun(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    val withIdx = children.indexWhere {
      case t: Tree if t.kind == TreeKind.Expr.RunWithBodyExpr => true
      case _ => false
    }
    if (withIdx < 0) return spaceJoin(children, Set.empty)

    val preamble    = children.take(withIdx)
    val handlers    = children.drop(withIdx)
    val preambleDoc = spaceJoin(preamble, Set.empty)
    localLayout(tree) {
      preambleDoc <+> Doc.column(col =>
        handlers.map(prettyChild).reduceLeftOption { (acc, h) =>
          acc <> Doc.nestAbsolute(col, line) <> h
        }.getOrElse(empty)
      )
    }
  }

  private def prettyRunWith(tree: Tree): Doc =
    spaceJoin(filterEmpty(tree.children), Set.empty)

  private def prettyHandler(tree: Tree): Doc =
    prettyBracket(tree, filterEmpty(tree.children),
      headerJoin = cs => spaceJoin(cs, Set.empty),
      formatBody = cs => joinWithGap(filterEmpty(cs), line))

  private def prettyIfThenElse(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    splitAtBracket(children) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(_, open, condChildren, close, tail)) =>

        val condDoc = condChildren.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

        val elseIdx = tail.indexWhere {
          case t: Token if t.kind == TokenKind.KeywordElse => true
          case _ => false
        }

        val thenChildren = if (elseIdx < 0) tail else tail.take(elseIdx)
        val elseChildren = if (elseIdx < 0) Array.empty[SyntaxTree.Child] else tail.drop(elseIdx + 1)

        val thenBody = joinWithGap(thenChildren)
        val elseBody = joinWithGap(elseChildren)

        val thenIsBlock = thenChildren.headOption.exists(isBlockExpr)
        val elseIsBlock = elseChildren.headOption.exists(isBlockExpr)
        val isElseIf    = elseChildren.headOption.exists(isIfThenElseExpr)

        val condPart = text("if") <+> text(open) <> condDoc <> text(close)
        val thenPart = if (thenIsBlock) condPart <+> thenBody else condPart <> nest(4, line <> thenBody)
        val thenEndsWithComment = thenChildren.lastOption.exists(endsWithComment)
        val elseConnector = if (thenEndsWithComment) hardline
                            else if (thenIsBlock)    space
                            else                     line

        localLayout(tree) {
          if (elseChildren.isEmpty) {
            thenPart
          } else if (isElseIf || elseIsBlock) {
            thenPart <> elseConnector <> text("else") <+> elseBody
          } else {
            thenPart <> elseConnector <> text("else") <> nest(4, line <> elseBody)
          }
        }
    }
  }

  private def prettyArrowRuleFragment(tree: Tree, headerJoin: Array[SyntaxTree.Child] => Doc): Doc = {
    val children = filterEmpty(tree.children)
    val arrowIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.ArrowThickR => true
      case _ => false
    }
    if (arrowIndex < 0) return prettyFallback(tree)

    val header = children.take(arrowIndex + 1)
    val body   = children.drop(arrowIndex + 1)

    val headerDoc = headerJoin(header)

    if (body.isEmpty) headerDoc
    else {
      val bodyDoc    = body.map(prettyChild).reduceLeftOption(_ <|> _).getOrElse(empty)
      val bodyIsBlock = body.headOption.exists(isBlockExpr)
      localLayout(tree) {
        if (bodyIsBlock) headerDoc <+> bodyDoc
        else headerDoc <> nest(4, line <> bodyDoc)
      }
    }
  }

  private def prettyMatchRuleFragment(tree: Tree): Doc =
    prettyArrowRuleFragment(tree, defaultHeaderJoin)

  private def prettyParYield(tree: Tree): Doc =
    splitAtBracket(filterEmpty(tree.children)) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, tail)) =>
        val headerDoc = defaultHeaderJoin(header)
        val bodyDoc = joinChildren(body,
          TokenKind.Semi -> (text(";") <> space))
        val tailDoc = defaultHeaderJoin(tail)
        val tailPart = if (tail.isEmpty) empty else space <> tailDoc
        val closeGap = if (body.nonEmpty && endsWithComment(body.last)) hardline else empty
        localLayout(tree) {
          headerDoc <+> text(open) <> bodyDoc <> closeGap <> text(close) <> tailPart
        }
    }

  private def prettyRestrictableChoose(tree: Tree): Doc =
    prettyBracket(tree, filterEmpty(tree.children),
      headerJoin = defaultHeaderJoin)

  private def prettyFor(tree: Tree): Doc =
    splitAtBracket(filterEmpty(tree.children)) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, tail)) =>
        val headerDoc  = defaultHeaderJoin(header)
        val bodyDoc    = joinChildren(body, TokenKind.Semi -> (text(";") <> line))
        val tailDoc    = defaultHeaderJoin(tail)
        val tailSuffix = if (tail.nonEmpty) space <> tailDoc else empty
        Doc.setLayout(Layout.MultiLine,
          headerDoc <+> text(open) <> nest(4, line <> bodyDoc) <> line <> text(close) <> tailSuffix
        )
    }

  private def prettyForFragment(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.ArrowThinL -> (space <> text("<-") <> space),
      TokenKind.Equal       -> (space <> text("=") <> space),
      TokenKind.KeywordIf   -> (text("if") <> space))

  private val bracketPairs: List[(TokenKind, TokenKind, String, String)] = List(
    (TokenKind.HashCurlyL, TokenKind.CurlyR,    "#{", "}"),
    (TokenKind.CurlyL,     TokenKind.CurlyR,    "{",  "}"),
    (TokenKind.ParenL,     TokenKind.ParenR,     "(",  ")"),
    (TokenKind.HashParenL, TokenKind.ParenR,     "#(", ")"),
    (TokenKind.BracketL,   TokenKind.BracketR,   "[",  "]")
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
    * Filters out empty [[SyntaxTree.Tree.children]].
    *
    * @param children the array of children to filter
    * @return a new array containing only non-empty children
    */
  private def filterEmpty(children: Array[SyntaxTree.Child]): Array[SyntaxTree.Child] =
    children.filter {
      case t: Tree if t.children.isEmpty => false
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
    * Core rendering for a bracket construct. This is where the decision point is.
    * Applies [[localLayout]] based on the source position of [[SyntaxTree.Tree]].
    *
    * @param tree      the tree used to determine single/multi-line layout
    * @param split     the pre-split bracket structure from [[splitAtBracket]]
    * @param headerJoin how to join children before the opening bracket
    * @param bodyDoc   the already-formatted body; [[Doc.Empty]] produces an empty bracket pair
    * @param tailDoc   the already-formatted tail after the closing bracket
    */
  private def renderBracket(
    tree: Tree,
    split: BracketSplit,
    headerJoin: Array[SyntaxTree.Child] => Doc = defaultHeaderJoin,
    bodyDoc: Doc,
    tailDoc: Doc = empty,
    flatPad: Doc = empty,
    nestLevel: Int = 4
  ): Doc = {
    val noGap = split.open == "[" || split.header.lastOption.exists {
      case token: Token => token.text.endsWith("#")
      case _ => false
    }
    val headerDoc = headerJoin(split.header)
    val openDoc =
      if (split.header.isEmpty) text(split.open)
      else if (noGap)           headerDoc <> text(split.open)
      else                      headerDoc <+> text(split.open)

    val pad = Doc.layoutChoice(flatPad, line)
    localLayout(tree) {
      bodyDoc match {
        case Doc.Empty => openDoc <> text(split.close) <> tailDoc
        case _         => openDoc <> nest(nestLevel, pad <> bodyDoc) <> pad <> text(split.close) <> tailDoc
      }
    }
  }

  /**
    * Extract annotations and doc comments from the beginning of the children.
    */
  private def extractAnnAndDoc(tree: Tree): (Doc, Array[SyntaxTree.Child]) = {
    val children = filterEmpty(tree.children)
    val (prefixChildren, rest) = children.span {
      case t: Tree if t.kind == TreeKind.AnnotationList => true
      case t: Tree if t.kind == TreeKind.Doc            => true
      case _ => false
    }
    val prefixDoc = hardStack(prefixChildren.map(prettyChild).toList)
    (prefixDoc, rest)
  }

  private def wrapWithAnn(tree: Tree, inner: Array[SyntaxTree.Child] => Doc): Doc = {
    val (annDoc, rest) = extractAnnAndDoc(tree)
    prepend(annDoc, inner(rest))
  }

  /**
    * Formatting for declarations with brackets (e.g. enum, instance, trait, struct, module).
    * Extracts annotations, then delegates to [[prettyBracket]].
    *
    * @param tree       the declaration tree
    * @param headerJoin how to join children before the opening bracket
    * @param formatBody formats the raw body children into a Doc
    * @param fallback   called when no bracket pair is found
    * @return the formatted declaration as Doc
    */
  private def prettyDeclBracket(
    tree: Tree,
    headerJoin: Array[SyntaxTree.Child] => Doc = defaultHeaderJoin,
    formatBody: Array[SyntaxTree.Child] => Doc = cs => joinWithGap(filterEmpty(cs)),
    fallback: Tree => Doc = prettyFallback,
    nestLevel: Int = 4
  ): Doc = wrapWithAnn(tree, rest =>
    prettyBracket(tree.copy(children = rest), rest, headerJoin, formatBody, fallback = fallback, nestLevel = nestLevel)
  )

  /**
    * Scans [[children]] for a bracket pair, formats body and tail with the provided
    * functions, and delegates to [[renderBracket]].
    *
    * @param tree       the tree used for layout; pass an annotation-stripped copy when applicable
    * @param children   the children to scan for a bracket pair
    * @param headerJoin how to join children before the opening bracket
    * @param formatBody formats the raw body children into a Doc
    * @param formatTail formats the raw tail children into a Doc
    * @param fallback   called when no bracket pair is found
    * @return the formatted construct as Doc
    */
  private def prettyBracket(
    tree: Tree,
    children: Array[SyntaxTree.Child],
    headerJoin: Array[SyntaxTree.Child] => Doc = defaultHeaderJoin,
    formatBody: Array[SyntaxTree.Child] => Doc = cs => joinWithGap(filterEmpty(cs)),
    formatTail: Array[SyntaxTree.Child] => Doc = cs => if (cs.isEmpty) empty else joinWithGap(cs),
    fallback: Tree => Doc = prettyFallback,
    flatPad: Doc = empty,
    nestLevel: Int = 4
  ): Doc = splitAtBracket(children) match {
    case None        => fallback(tree)
    case Some(split) =>
      val tailContent = formatTail(split.tail)
      val tailDoc = tailContent match {
        case Doc.Empty => empty
        case _ =>
          val ci = children.length - split.tail.length - 1
          val sep = if (ci >= 0 && ci < children.length) {
            children(ci) match {
              case closeToken: Token =>
                split.tail.headOption.flatMap(leftMostToken) match {
                  case Some(first) if closeToken.end.lineOneIndexed == first.start.lineOneIndexed =>
                    space
                  case Some(first) if first.start.lineOneIndexed - closeToken.end.lineOneIndexed > 1 =>
                    hardline <> hardline
                  case _ => hardline
                }
              case _ => space
            }
          } else space
          sep <> tailContent
      }
      renderBracket(tree, split, headerJoin,
        bodyDoc   = formatBody(split.body),
        tailDoc   = tailDoc,
        flatPad   = flatPad,
        nestLevel = nestLevel)
  }

  /**
    * Formatting for constructs with comma-separated bodies (e.g literal vectors, lists, sets, maps, arrays).
    * Finds the first matching bracket pair and joins body children with commas and bars, without filtering out empty children.
    *
    * @param tree the tree to format
    * @return the formatted construct as Doc
    */
  private def prettyCommaBracket(tree: Tree): Doc =
    prettyBracket(tree, tree.children, formatBody = commaBodyJoin)

  private def prettySpacedCommaBracket(tree: Tree): Doc =
    splitAtBracket(tree.children) match {
      case None        => prettyFallback(tree)
      case Some(split) => renderBracket(tree, split,
        bodyDoc = commaBodyJoin(split.body),
        flatPad = space)
    }

  private def prettyRecordOpExtend(tree: Tree): Doc =
    joinChildren(tree.children,
      TokenKind.Plus  -> text("+"),
      TokenKind.Equal -> (space <> text("=") <> space))

  private def prettyRecordOpRestrict(tree: Tree): Doc =
    joinChildren(tree.children, TokenKind.Minus -> text("-"))

  private def prettyRecordFieldAssign(tree: Tree): Doc =
    joinChildren(tree.children, TokenKind.Equal -> (space <> text("=") <> space))

  private def commaBodyJoin(children: Array[SyntaxTree.Child]): Doc =
    joinChildren(children,
      TokenKind.Comma -> (text(",") <> line),
      TokenKind.Bar   -> (space <> text("|") <> space))

  /**
    * Formatting for enum and restrictable enum declarations.
    */
  private def prettyEnum(tree: Tree): Doc = wrapWithAnn(tree, rest => {
    val hasBraces = rest.exists {
      case token: Token => token.kind == TokenKind.CurlyL || token.kind == TokenKind.CurlyR
      case _ => false
    }

    if (!hasBraces)
      spaceJoin(filterEmpty(rest), noSpacePairs = Set(
        (TreeKind.Ident, TreeKind.TypeParameterList),
        (TreeKind.TypeParameterList, TreeKind.CaseBody),
        (TreeKind.Ident, TreeKind.CaseBody)
      ))
    else
      prettyBracket(tree.copy(children = rest), filterEmpty(rest),
        headerJoin = declHeaderJoin,
        formatBody = body => joinWithGap(filterEmpty(body))
      )
  })

  private def prettyCase(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    if (children.isEmpty) return prettyFallback(tree)

    val caseIdx = children.indexWhere {
      case t: Token if t.kind == TokenKind.KeywordCase => true
      case _ => false
    }
    if (caseIdx < 0) return prettyFallback(tree)

    val leading = children.slice(0, caseIdx)
    val body    = children.drop(caseIdx)

    val bodyDoc = spaceJoin(body, noSpacePairs = Set((TreeKind.Ident, TreeKind.CaseBody)))

    if (leading.isEmpty) {
      bodyDoc
    } else {
      val comments = leading.filter {
        case t: Token if t.kind == TokenKind.Comma => false
        case _ => true
      }
      comments.map(prettyChild).reduceLeftOption(_ <+> _) match {
        case Some(commentDoc) => space <> commentDoc <> hardline <> bodyDoc
        case None             => hardline <> bodyDoc
      }
    }
  }

  private def prettyCaseBody(tree: Tree): Doc =
    splitAtBracket(tree.children) match {
      case None => prettyFallback(tree)
      case Some(split) =>
        val bodyDoc = commaBodyJoin(split.body)
        val tailDoc = if (split.tail.isEmpty) empty else space <> joinWithGap(split.tail)
        val bodyStartsWithBrace = split.body.headOption.exists {
          case t: Token if t.kind == TokenKind.CurlyL => true
          case t: Tree  => leftMostToken(t).exists(_.kind == TokenKind.CurlyL)
          case _        => false
        }
        if (bodyStartsWithBrace)
          localLayout(tree) { text(split.open) <> bodyDoc <> text(split.close) <> tailDoc }
        else
          renderBracket(tree, split, bodyDoc = bodyDoc, tailDoc = tailDoc)
    }

  private def prettyInstance(tree: Tree): Doc = {
    val nestLevel = {
      val children = filterEmpty(tree.children)
      val curlLIdx = children.indexWhere { case t: Token if t.kind == TokenKind.CurlyL => true; case _ => false }
      val header   = if (curlLIdx > 0) children.take(curlLIdx) else children
      val first    = header.headOption.flatMap(leftMostToken)
      val last     = header.lastOption.flatMap(rightMostToken)
      (first, last) match {
        case (Some(f), Some(l)) if f.start.lineOneIndexed != l.end.lineOneIndexed => 8
        case _                                                                     => 4
      }
    }
    prettyDeclBracket(tree, headerJoin = instanceHeaderJoin, nestLevel = nestLevel)
  }

  private def prettyEffect(tree: Tree): Doc =
    prettyDeclBracket(tree, headerJoin = defaultHeaderJoin)

  private def prettyTrait(tree: Tree): Doc =
    prettyDeclBracket(tree, headerJoin = declHeaderJoin)

  private def prettyStruct(tree: Tree): Doc =
    prettyDeclBracket(tree,
      headerJoin = declHeaderJoin,
      formatBody = alignedStructBody)

  private def structFieldPreColonWidth(field: Tree): Int = {
    val children = filterEmpty(field.children)
    val idx = children.indexWhere { case t: Token if t.kind == TokenKind.Colon => true; case _ => false }
    if (idx < 0) 0
    else pretty(Layout.SingleLine, spaceJoin(children.take(idx), noSpacePairs = Set.empty)).length
  }

  private def alignedStructField(tree: Tree, maxWidth: Int): Doc = {
    val children = filterEmpty(tree.children)
    val idx = children.indexWhere { case t: Token if t.kind == TokenKind.Colon => true; case _ => false }
    if (idx < 0) return prettyFallback(tree)
    val nameDoc = spaceJoin(children.take(idx), noSpacePairs = Set.empty)
    val typeDoc = joinWithGap(children.drop(idx + 1))
    Doc.fill(maxWidth + 1, nameDoc <> text(":")) <+> typeDoc
  }

  private def alignedStructBody(children: Array[SyntaxTree.Child]): Doc = {
    val filtered = filterEmpty(children)
    val fields   = filtered.collect { case t: Tree if t.kind == TreeKind.StructField => t }
    val maxWidth = fields.map(structFieldPreColonWidth).maxOption.getOrElse(0)
    val docs = filtered.map {
      case t: Tree if t.kind == TreeKind.StructField => alignedStructField(t, maxWidth)
      case other                                      => prettyChild(other)
    }
    val pairs = filtered.zip(docs)
    if (pairs.isEmpty) return empty
    pairs.sliding(2).foldLeft(pairs.head._2) {
      case (acc, Array((prev, _), (next, nextDoc))) => acc <> structuralGap(prev, next) <> nextDoc
      case (acc, _)                                 => acc
    }
  }

  /**
    * Formatting for match and select expressions.
    *
    * @param tree the match or select expression tree
    * @return the formatted match or select expression as Doc
    */
  private val ArrowRuleKinds: Set[TreeKind] = Set(
    TreeKind.Expr.MatchRuleFragment,
    TreeKind.Expr.ExtMatchRuleFragment,
    TreeKind.Expr.SelectRuleFragment,
    TreeKind.Expr.SelectRuleDefaultFragment
  )

  private def prettyMatch(tree: Tree): Doc =
    prettyBracket(tree, filterEmpty(tree.children),
      headerJoin = defaultHeaderJoin,
      formatBody = alignedMatchBody)

  private def alignedMatchBody(children: Array[SyntaxTree.Child]): Doc = {
    val filtered = filterEmpty(children)
    val rules    = filtered.collect { case t: Tree if ArrowRuleKinds.contains(t.kind) => t }
    val maxWidth = rules.map(arrowPatternWidth).maxOption.getOrElse(0)
    val docs = filtered.map {
      case t: Tree if ArrowRuleKinds.contains(t.kind) => alignedArrowRule(t, maxWidth)
      case other                                       => prettyChild(other)
    }
    val pairs = filtered.zip(docs)
    if (pairs.isEmpty) return empty
    pairs.sliding(2).foldLeft(pairs.head._2) {
      case (acc, Array((prev, _), (next, nextDoc))) => acc <> structuralGap(prev, next) <> nextDoc
      case (acc, _)                                 => acc
    }
  }

  private def arrowPatternWidth(rule: Tree): Int = {
    val children = filterEmpty(rule.children)
    val idx = children.indexWhere { case t: Token if t.kind == TokenKind.ArrowThickR => true; case _ => false }
    if (idx < 0) 0
    else pretty(Layout.SingleLine, defaultHeaderJoin(children.take(idx))).length
  }

  private def alignedArrowRule(tree: Tree, maxWidth: Int): Doc = {
    val children = filterEmpty(tree.children)
    val idx = children.indexWhere { case t: Token if t.kind == TokenKind.ArrowThickR => true; case _ => false }
    if (idx < 0) return prettyFallback(tree)

    val patternPart = children.take(idx)
    val bodyPart    = children.drop(idx + 1)
    val patternDoc  = defaultHeaderJoin(patternPart)
    val bodyDoc     = joinWithGap(bodyPart)
    val bodyIsBlock = bodyPart.headOption.exists(isBlockExpr)

    if (!bodyIsBlock && layoutOf(tree) == Layout.MultiLine)
      Doc.fill(maxWidth, patternDoc) <+> text("=>") <> nest(4, hardline <> bodyDoc)
    else
      Doc.fill(maxWidth, patternDoc) <+> text("=>") <+> bodyDoc
  }

  private def prettyTry(tree: Tree): Doc =
    spaceJoin(filterEmpty(tree.children), Set.empty)

  private def prettyTryCatchRuleFragment(tree: Tree): Doc =
    prettyArrowRuleFragment(tree, cs => spaceJoin(cs, noSpacePairs = Set.empty, noSpaceBefore = Set(TokenKind.Colon)))

  private def prettyAscribe(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.Colon -> (text(":") <> space))

  private def prettyUncheckedCast(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.KeywordUncheckedCast -> text("unchecked_cast"),
      TokenKind.KeywordAs            -> (space <> text("as") <> space),
      TokenKind.Backslash            -> (space <> text("\\") <> space))

  private def prettyCheckedCast(tree: Tree, kind: TokenKind, kw: String): Doc =
    joinChildren(filterEmpty(tree.children),
      kind                -> text(kw),
      TokenKind.Backslash -> (space <> text("\\") <> space))

  private def prettyUnsafe(tree: Tree): Doc =
    prettyBracket(tree, filterEmpty(tree.children), flatPad = space)

  private def prettyTypeConcat(tree: Tree): Doc =
    filterEmpty(tree.children).map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyConstraintList(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    val rest = children.filterNot {
      case t: Token => t.kind == TokenKind.KeywordWith
      case _        => false
    }
    if (rest.isEmpty) return prettyFallback(tree)
    val bodyDoc = joinChildren(rest, TokenKind.Comma -> (text(",") <> line))
    localLayout(tree) {
      text("with") <> nest(4, line <> bodyDoc)
    }
  }

  private def prettyFixpointConstraintSet(tree: Tree): Doc =
    splitAtBracket(filterEmpty(tree.children)) match {
      case None => prettyFallback(tree)
      case Some(split) =>
        val bodyDoc = joinWithGap(filterEmpty(split.body), line)
        val pad = Doc.layoutChoice(space, line)
        localLayout(tree) {
          bodyDoc match {
            case Doc.Empty => text(split.open) <> text(split.close)
            case _         => text(split.open) <> nest(4, pad <> bodyDoc) <> pad <> text(split.close)
          }
        }
    }

  private def prettyFixpointConstraint(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    val colonIdx = children.indexWhere {
      case t: Token if t.kind == TokenKind.ColonMinus => true
      case _ => false
    }
    if (colonIdx < 0) {
      joinChildren(children, TokenKind.DotWhiteSpace -> (text(".") <> space))
    } else {
      val head    = children.take(colonIdx)
      val body    = children.drop(colonIdx + 1)
      val headDoc = head.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
      val bodyDoc = joinChildren(body,
        TokenKind.Comma         -> (text(",") <> line),
        TokenKind.DotWhiteSpace -> (text(".") <> space))
      localLayout(tree) {
        headDoc <+> text(":-") <+> nest(4, bodyDoc)
      }
    }
  }

  private def prettyFixpointQuery(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.KeywordQuery  -> (text("query") <> space),
      TokenKind.KeywordPQuery -> (text("pquery") <> space),
      TokenKind.Comma         -> (text(",") <> space))

  private def prettyFixpointSelect(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.KeywordSelect -> (space <> text("select") <> space),
      TokenKind.Comma         -> (text(",") <> space))

  private def prettyFixpointFromFragment(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.KeywordFrom -> (space <> text("from") <> space),
      TokenKind.Comma       -> (text(",") <> space))

  private def prettyFixpointWhere(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.KeywordWhere -> (space <> text("where") <> space))

  private def prettyFixpointWith(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.KeywordWith -> (space <> text("with") <> space),
      TokenKind.Comma       -> (text(",") <> space))

  private def prettyFixpointInject(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.KeywordInject -> (text("inject") <> space),
      TokenKind.KeywordInto   -> (space <> text("into") <> space),
      TokenKind.Comma         -> (text(",") <> space))

  private def prettyFixpointSolve(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.KeywordSolve   -> (text("solve") <> space),
      TokenKind.KeywordPSolve  -> (text("psolve") <> space),
      TokenKind.KeywordProject -> (space <> text("project") <> space),
      TokenKind.Comma          -> (text(",") <> space))

  private def prettyFixpointLambda(tree: Tree): Doc =
    joinChildren(filterEmpty(tree.children),
      TokenKind.ArrowThinRWhitespace -> (space <> text("->") <> space))

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

  private def instanceHeaderJoin(cs: Array[SyntaxTree.Child]): Doc =
    spaceJoin(cs,
      noSpacePairs = Set((TreeKind.Ident, TreeKind.TypeParameterList)),
      noSpaceBefore = Set(TokenKind.BracketL, TokenKind.BracketR),
      noSpaceAfter  = Set(TokenKind.BracketL))

  /**
    * Formatting for module declarations.
    * Preserves blank lines from the source between declarations inside the module body.
    *
    * @param tree the module declaration tree
    * @return the formatted module declaration as Doc
    */
  private def prettyModule(tree: Tree): Doc = {
    val openToken = filterEmpty(tree.children).collectFirst {
      case t: Token if t.kind == TokenKind.CurlyL => t
    }
    prettyDeclBracket(tree,
      formatBody = cs => {
        val filtered = filterEmpty(cs)
        if (filtered.isEmpty) empty
        else {
          val hasLeadingBlank = openToken.flatMap(open =>
            filtered.headOption.flatMap(leftMostToken).map(first =>
              first.start.lineOneIndexed - open.end.lineOneIndexed > 1
            )
          ).getOrElse(false)
          val base = joinWithPreservedBlanks(filtered, hardline)
          if (hasLeadingBlank) hardline <> base else base
        }
      })
  }

  /**
    * Joins children separated by [[base]], upgrading to a blank line when
    * the source had a blank line between two adjacent children.
    * At most one blank line is ever emitted.
    *
    * @param children the children to join
    * @param base the separator used when no blank line was present in the source
    * @return the joined document
    */
  private def joinWithPreservedBlanks(children: Array[SyntaxTree.Child], base: Doc): Doc = {
    if (children.isEmpty) return empty
    children.sliding(2).foldLeft(prettyChild(children.head)) {
      case (acc, Array(prev, next)) =>
        val sep = if (hadBlankLineBetween(prev, next)) hardline <> hardline else base
        acc <> sep <> prettyChild(next)
      case (acc, _) => acc
    }
  }

  /**
    * Returns true when the source had at least one blank line between two adjacent children.
    */
  private def hadBlankLineBetween(prev: SyntaxTree.Child, next: SyntaxTree.Child): Boolean =
    (rightMostToken(prev), leftMostToken(next)) match {
      case (Some(p), Some(n)) => n.start.lineOneIndexed - p.end.lineOneIndexed > 1
      case _ => false
    }

  private def buildSig(parts: Array[SyntaxTree.Child]): Doc =
    spaceJoin(parts,
      noSpacePairs = Set((TreeKind.Ident, TreeKind.ParameterList)),
      noSpaceBefore = Set(TokenKind.Colon))

  /**
    * Formatting for definitions.
    *
    * @param tree the definition tree
    * @return the formatted definition as Doc
    */
  private def prettyDef(tree: Tree): Doc = {
    val (annDoc, rest) = extractAnnAndDoc(tree)

    val eqIndex = rest.indexWhere {
      case token: Token if token.kind == TokenKind.Equal => true
      case _ => false
    }

    if (eqIndex < 0) return prepend(annDoc, buildSig(rest))

    val sigParts  = rest.take(eqIndex)
    val bodyParts = rest.drop(eqIndex + 1)

    val sig = buildSig(sigParts)

    val body = bodyParts.map(prettyChild)
      .reduceLeftOption(_ <> _)
      .getOrElse(empty)

    val bodyIsBlock = bodyParts.exists(isBracedExpr) ||
      bodyParts.exists(c => leftMostToken(c).exists(tok => bracketPairs.exists(_._1 == tok.kind)))

    val defDoc = localLayout(tree) {
      if (bodyIsBlock) sig <+> text("=") <+> body
      else sig <+> text("=") <> nest(4, line <> body)
    }

    prepend(annDoc, defDoc)
  }

  /**
    * Formatting for local def expressions.
    *
    * @param tree the local def expression tree
    * @return the formatted local def expression as Doc
    */
  private def prettyLocalDef(tree: Tree): Doc = {
    val (annDoc, rest) = extractAnnAndDoc(tree)
    val eqIndex = rest.indexWhere {
      case token: Token if token.kind == TokenKind.Equal => true
      case _ => false
    }
    if (eqIndex < 0) return prepend(annDoc, buildSig(rest))

    val sigParts  = rest.take(eqIndex)
    val bodyParts = rest.drop(eqIndex + 1)

    val sig = buildSig(sigParts)

    bodyParts.headOption.flatMap(unwrapToStatement) match {
      case Some(stmt) =>
        val stmtChildren = filterEmpty(stmt.children)
        val semiIdx = stmtChildren.indexWhere {
          case t: Token if t.kind == TokenKind.Semi => true
          case _ => false
        }
        if (semiIdx < 0) return prettyDef(tree)

        val bodyExprs = stmtChildren.take(semiIdx)
        val contExprs = stmtChildren.drop(semiIdx + 1)
        val bodyDoc = bodyExprs.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
        val contDoc = contExprs.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
        val bodyIsBraced = bodyExprs.headOption.exists(isBracedExpr)

        val contSep = {
          val hasCont = semiIdx + 1 < stmtChildren.length
          val extraBlank = hasCont && hadBlankLineBetween(stmtChildren(semiIdx), stmtChildren(semiIdx + 1))
          if (extraBlank) hardline <> hardline else hardline
        }

        val defLine  = sigParts.headOption.flatMap(leftMostToken).map(_.start.lineOneIndexed)
        val bodyLine = bodyExprs.lastOption.flatMap(rightMostToken).map(_.end.lineOneIndexed)
        val defLayout = (defLine, bodyLine) match {
          case (Some(d), Some(b)) => if (d == b) Layout.SingleLine else Layout.MultiLine
          case _                  => Layout.MultiLine
        }
        val defDoc = Doc.setLayout(defLayout, {
          val bodyPart =
            if (bodyIsBraced) sig <+> text("=") <+> bodyDoc
            else              sig <+> text("=") <> nest(4, line <> bodyDoc)
          if (contExprs.isEmpty) bodyPart <> text(";")
          else bodyPart <> text(";") <> contSep <> contDoc
        })
        prepend(annDoc, defDoc)

      case None => prettyDef(tree)
    }
  }

  private def unwrapToStatement(child: SyntaxTree.Child): Option[Tree] = child match {
    case t: Tree if t.kind == TreeKind.Expr.Statement =>
      Some(t)
    case t: Tree if t.kind == TreeKind.Expr.Expr =>
      filterEmpty(t.children).headOption.flatMap(unwrapToStatement)
    case _ => None
  }

  /**
    * Formatting for type alias.
    *
    * @param tree the type alias tree
    * @return the formatted type alias as Doc
    */
  private def prettyTypeAlias(tree: Tree): Doc = wrapWithAnn(tree, rest =>
    spaceJoin(rest,
      noSpacePairs  = Set((TreeKind.Ident, TreeKind.TypeParameterList)),
      noSpaceBefore = Set(TokenKind.BracketL, TokenKind.BracketR, TokenKind.Colon),
      noSpaceAfter  = Set(TokenKind.BracketL))
  )

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
      val sep = if (endsWithComment(tree.children(1))) hardline else space
      localLayout(tree) {
        if (endsWithClose)
          parts(0) <> space <> parts(1) <> sep <> parts(2)
        else
          parts(0) <> nest(4, line <> parts(1) <> sep <> parts(2))
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
    prettyBracket(tree, filterEmpty(tree.children), flatPad = space)

  /**
    * Formatting for statements.
    *
    * @param tree the statement tree
    * @return the formatted statement as Doc
    */
  private def prettyStatement(tree: Tree): Doc = localLayout(tree) {
    joinSemiPreservingBlanks(tree.children)
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
      TokenKind.Colon      -> (text(":") <> space),
      TokenKind.Equal      -> (space <> text("=") <> space),
      TokenKind.Semi       -> (text(";") <> line))
  }

  /**
    * Joins children, replacing each `Semi` token with `";"` followed by a newline.
    * Adds an extra blank line after the semicolon when the source had one.
    *
    * @param children the children to join
    * @return the joined document
    */
  private def joinSemiPreservingBlanks(children: Array[SyntaxTree.Child]): Doc = {
    if (children.isEmpty) return empty
    var i = 0
    var acc: Doc = empty
    var prev: Option[SyntaxTree.Child] = None
    while (i < children.length) {
      children(i) match {
        case token: Token if token.kind == TokenKind.Semi =>
          val nextOpt = if (i + 1 < children.length) Some(children(i + 1)) else None
          val hasSameLineComment = nextOpt.flatMap(leftMostToken).exists { t =>
            isCommentKind(t.kind) && t.start.lineOneIndexed == token.end.lineOneIndexed
          }
          val gap = if (prev.exists(endsWithComment)) hardline else empty
          if (hasSameLineComment) {
            acc = acc <> gap <> text(";") <> space
          } else {
            val extraBlank = nextOpt.exists(next => hadBlankLineBetween(token, next))
            val lineSep = if (extraBlank) line <> Doc.layoutChoice(empty, hardline) else line
            acc = acc <> gap <> text(";") <> lineSep
          }
          prev = Some(token)
          i += 1
        case child =>
          val gap = if (prev.exists(endsWithComment)) {
            if (hadBlankLineBetween(prev.get, child)) hardline <> hardline else hardline
          } else empty
          acc = acc <> gap <> prettyChild(child)
          prev = Some(child)
          i += 1
      }
    }
    acc
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
        val headerDoc = defaultHeaderJoin(header)
        val tailDoc = tail.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
        val tailStart = tail.headOption.flatMap(leftMostToken)
        val isBlock = tailStart.exists(t => t.kind == TokenKind.CurlyL || t.kind == TokenKind.ParenL)
        val tailSep = if (isBlock) space <> tailDoc else nest(4, line <> tailDoc)
        val headerFitsOnOneLine = body.isEmpty || {
          val firstLine = body.headOption.flatMap(leftMostToken).map(_.start.lineOneIndexed)
          val lastLine = body.lastOption.flatMap(rightMostToken).map(_.end.lineOneIndexed)
          firstLine == lastLine
        }
        if (headerFitsOnOneLine) {
          val bodyDoc = joinChildren(body, TokenKind.Semi -> (text(";") <> space))
          headerDoc <+> text(open) <> bodyDoc <> text(close) <> tailSep
        } else {
          val bodyDoc = joinWithGap(body)
          val closeSep = if (body.nonEmpty && endsWithComment(body.last)) hardline
                         else Doc.layoutChoice(empty, line)
          headerDoc <+> text(open) <>
            nest(4, Doc.layoutChoice(empty, line) <> bodyDoc) <>
            closeSep <>
            text(close) <>
            tailSep
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
    val bodyOpensWithBracket = body.headOption.exists(isBracedExpr) ||
      body.headOption.exists(c => leftMostToken(c).exists(tok => bracketPairs.exists(_._1 == tok.kind)))

    localLayout(tree) {
      if (bodyOpensWithBracket) headerDoc <+> text("->") <+> bodyDoc
      else headerDoc <+> text("->") <> nest(4, line <> bodyDoc)
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
        val inner = joinWithGap(filterEmpty(body), line)
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
  private def prettyNewKeyword(tree: Tree): Doc =
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

  // This must hold all keywords.
  private val ReservedKeywords: Set[String] = Set(
    "as", "assume", "comptime", "do", "else", "enum", "export",
    "external", "false", "fixpoint", "for", "if", "import", "instance",
    "intrinsic", "match", "new", "select", "struct", "then", "trait",
    "true", "type", "use", "with", "run", "and", "or", "not", "xor", "lazy", "unchecked_cast", "checked_cast",
    "unchecked_coerce", "checked_coerce", "inject", "into", "solve", "psolve", "project", "query", "pquery"
  )

  private def prettyIdent(tree: Tree): Doc =
    tree.children.foldLeft((empty: Doc, Option.empty[SyntaxTree.Child])) {
      case ((acc, prev), child) =>
        val childDoc = child match {
          case token: Token if ReservedKeywords.contains(token.text) => text("$" + token.text)
          case c => prettyChild(c)
        }
        val gap = prev.fold(empty)(p => structuralGap(p, child))
        (acc <> gap <> childDoc, Some(child))
    }._1

  private def prettyStringInterpolation(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyOperator(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  private def prettyQName(tree: Tree): Doc = prettyOperator(tree)

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
    prettyBracket(tree, tree.children,
      formatBody = cs => joinChildren(cs, TokenKind.Comma -> (text(",") <> line)))

  /**
    * Formatting for individual parameters.
    * Joins children with spaces, replacing colons with ": " and surrounding them with spaces
    *
    * @param tree the parameter tree
    * @return the formatted parameter tree as Doc
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

    if (!hasArgs) {
      splitAtBracket(filterEmpty(tree.children)) match {
        case None => return text("(") <> text(")")
        case Some(BracketSplit(_, _, _, _, tail)) =>
          val tailDoc = if (tail.isEmpty) empty else space <> joinWithGap(tail)
          return text("(") <> text(")") <> tailDoc
      }
    }

    splitAtBracket(filterEmpty(tree.children)) match {
      case None => prettyFallback(tree)
      case Some(BracketSplit(header, open, body, close, tail)) =>
        val bodyDoc = commaBodyJoin(body)
        val openDoc =
          if (header.isEmpty) text(open)
          else defaultHeaderJoin(header) <> text(open)
        val tailDoc = if (tail.isEmpty) empty
        else {
          space <> joinWithGap(tail)
        }
        val args = body.collect { case t: Tree if t.kind == TreeKind.Argument || t.kind == TreeKind.ArgumentNamed => t }
        val singleBlockArg = args.length == 1 &&
          filterEmpty(args(0).children).headOption.exists(c =>
            leftMostToken(c).exists(tok => tok.kind == TokenKind.CurlyL || tok.kind == TokenKind.HashCurlyL))
        localLayout(tree) {
          if (singleBlockArg)
            openDoc <> bodyDoc <> text(close) <> tailDoc
          else
            openDoc <> nest(4, Doc.layoutChoice(empty, line) <> bodyDoc) <> Doc.layoutChoice(empty, line) <> text(close) <> tailDoc
        }
    }
  }

  /**
    * Formatting for the root of the syntax tree.
    * Preserves blank lines from the source (at most one blank line between declarations).
    *
    * @param tree the root tree
    * @return the formatted root as Doc
    */
  private def prettyRoot(tree: Tree): Doc = {
    val children = filterEmpty(tree.children)
    if (children.isEmpty) return empty
    joinWithPreservedBlanks(children, hardline)
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
    var i = 0
    var acc: Doc = empty
    var prev: Option[SyntaxTree.Child] = None
    var prevReplEndsWithLine = false
    while (i < children.length) {
      val child = children(i)
      child match {
        case token: Token if replMap.contains(token.kind) =>
          val gap = if (prev.exists(endsWithComment)) hardline else empty
          val replDoc = replMap(token.kind)
          val nextOpt = if (i + 1 < children.length) Some(children(i + 1)) else None
          val hasSameLineComment = (token.kind == TokenKind.Semi || token.kind == TokenKind.Comma) && {
            nextOpt.flatMap(leftMostToken).exists { t =>
              isCommentKind(t.kind) && t.start.lineOneIndexed == token.end.lineOneIndexed
            }
          }
          if (hasSameLineComment) {
            acc = acc <> gap <> text(token.text) <> space
            prevReplEndsWithLine = false
          } else {
            acc = acc <> gap <> replDoc
            prevReplEndsWithLine = docEndsWithLine(replDoc)
          }
          prev = Some(token)
          i += 1
        case _ =>
          val isComment = child match { case t: Token => isCommentKind(t.kind); case _ => false }
          val sameLine = isComment && prev.flatMap(rightMostToken).exists(p =>
            leftMostToken(child).exists(_.start.lineOneIndexed == p.end.lineOneIndexed))
          val blank = prev.exists(p => hadBlankLineBetween(p, child))
          val gap = if (prevReplEndsWithLine) {
            if (blank) hardline else empty
          } else if (prev.exists(endsWithComment)) {
            if (blank) hardline <> hardline else hardline
          } else if (sameLine) space
          else if (isComment) {
            if (blank) hardline <> hardline else hardline
          } else empty
          prevReplEndsWithLine = false
          acc = acc <> gap <> prettyChild(child)
          prev = Some(child)
          i += 1
      }
    }
    acc
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
      joinWithGap(children)
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
    TokenKind.ParenL,
    TokenKind.Hash,
    TokenKind.Dot,
    TokenKind.DotWhiteSpace,
    TokenKind.Comma,
    TokenKind.Semi
  )

  /**
    * Gap between two children. Handles interleaved comments that remain
    * in the tree, and preserves blank lines from the source.
    */
  private def structuralGap(prev: SyntaxTree.Child, next: SyntaxTree.Child): Doc = {
    val prevKind = rightMostToken(prev).map(_.kind)
    val nextKind = leftMostToken(next).map(_.kind)

    val afterComment = prevKind.exists(isCommentKind)
    if (afterComment) return if (hadBlankLineBetween(prev, next)) hardline <> hardline else hardline

    val nextIsComment = nextKind.exists(isCommentKind)
    if (nextIsComment) {
      val sameLine = (rightMostToken(prev), leftMostToken(next)) match {
        case (Some(p), Some(n)) => p.end.lineOneIndexed == n.start.lineOneIndexed
        case _ => false
      }
      if (sameLine) return space
      return if (hadBlankLineBetween(prev, next)) hardline <> hardline else hardline
    }

    val afterSeparator = prevKind.exists(k => k == TokenKind.Comma || k == TokenKind.Semi)
    val tight = !afterSeparator && (prevKind.exists(TightAfter.contains) ||
      nextKind.exists(TightBefore.contains))
    if (tight) empty
    else if (hadBlankLineBetween(prev, next)) line <> Doc.layoutChoice(empty, hardline)
    else line
  }

  private def joinWithGap(children: Array[SyntaxTree.Child]): Doc = {
    if (children.isEmpty) return empty
    children.sliding(2).foldLeft(prettyChild(children.head)) {
      case (acc, Array(prev, next)) => acc <> structuralGap(prev, next) <> prettyChild(next)
      case (acc, _) => acc
    }
  }

  private def endsWithComment(child: SyntaxTree.Child): Boolean =
    rightMostToken(child).exists(t => isCommentKind(t.kind))

  private def docEndsWithLine(doc: Doc): Boolean = doc match {
    case Doc.Line               => true
    case Doc.HardLine           => true
    case Doc.Concat(_, right)    => docEndsWithLine(right)
    case Doc.Nest(_, inner)      => docEndsWithLine(inner)
    case Doc.SetLayout(_, inner) => docEndsWithLine(inner)
    case Doc.LayoutChoice(s, m)  => docEndsWithLine(s) || docEndsWithLine(m)
    case _                       => false
  }

  /**
    * Like [[joinWithGap]] but uses a fixed `gap` instead of [[structuralGap]].
    * Still handles interleaved comments and preserves blank lines from the source.
    */
  private def joinWithGap(children: Array[SyntaxTree.Child], gap: Doc): Doc = {
    if (children.isEmpty) return empty
    children.sliding(2).foldLeft(prettyChild(children.head)) {
      case (acc, Array(prev, next)) =>
        val nextStartsWithComment = leftMostToken(next).exists(t => isCommentKind(t.kind))
        val sameLine = nextStartsWithComment && {
          (rightMostToken(prev), leftMostToken(next)) match {
            case (Some(p), Some(n)) => p.end.lineOneIndexed == n.start.lineOneIndexed
            case _                  => false
          }
        }
        val g = if (endsWithComment(prev))
          if (hadBlankLineBetween(prev, next)) hardline <> hardline else hardline
                else if (nextStartsWithComment && !sameLine)
                  if (hadBlankLineBetween(prev, next)) hardline <> hardline else hardline
                else if (sameLine) space
                else if (hadBlankLineBetween(prev, next)) gap <> Doc.layoutChoice(empty, hardline)
                else gap
        acc <> g <> prettyChild(next)
      case (acc, _) => acc
    }
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
    noSpaceBefore: Set[TokenKind] = Set.empty,
    noSpaceAfter: Set[TokenKind] = Set.empty
  ): Doc = {
    if (children.isEmpty) return empty

    children.sliding(2).foldLeft(prettyChild(children.head)) {
      case (acc, Array(prev, next)) =>
        val noSpace = (prev, next) match {
          case (p: Tree, n: Tree) if noSpacePairs.contains((p.kind, n.kind)) => true
          case (_, token: Token) if noSpaceBefore.contains(token.kind)       => true
          case (token: Token, _) if noSpaceAfter.contains(token.kind)        => true
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
    * @param body the main body to return if the prefix is not present, or to append to the prefix if it is present
    * @return the combined prefix and body if the prefix is present, otherwise just the body
    */
  private def prepend(prefix: Doc, body: Doc): Doc = prefix match {
    case Doc.Empty => body
    case _         => prefix <> hardline <> body
  }

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

  private def exprMatches(child: SyntaxTree.Child, pred: Tree => Boolean): Boolean = child match {
    case t: Tree if t.kind == TreeKind.Expr.Expr => t.children.exists(c => exprMatches(c, pred))
    case t: Tree => pred(t)
    case _       => false
  }

  private val BracedKinds: Set[TreeKind] = Set(
    TreeKind.Expr.Block,
    TreeKind.Expr.Match,
    TreeKind.Expr.ExtMatch,
    TreeKind.Expr.Select,
    TreeKind.Expr.RestrictableChoose,
    TreeKind.Expr.RestrictableChooseStar,
    TreeKind.Expr.Handler,
    TreeKind.Expr.Try,
    TreeKind.Expr.Lambda,
    TreeKind.Expr.LambdaMatch,
    TreeKind.Expr.LambdaExtMatch
  )

  private def isBlockExpr(child: SyntaxTree.Child): Boolean =
    exprMatches(child, _.kind == TreeKind.Expr.Block)

  private def isBracedExpr(child: SyntaxTree.Child): Boolean =
    exprMatches(child, t => BracedKinds.contains(t.kind))

  private def isIfThenElseExpr(child: SyntaxTree.Child): Boolean =
    exprMatches(child, _.kind == TreeKind.Expr.IfThenElse)

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
