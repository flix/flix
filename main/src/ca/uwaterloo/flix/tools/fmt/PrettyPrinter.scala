package ca.uwaterloo.flix.tools.fmt

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Tree, TreeKind}
import ca.uwaterloo.flix.tools.fmt.Doc.*

object PrettyPrinter {

  sealed trait Layout
  private object Layout {
    case object SingleLine extends Layout
    case object MultiLine extends Layout
  }

  private def layoutOf(tree: Tree): Layout = {
    if (tree.loc.isSingleLine) Layout.SingleLine
    else Layout.MultiLine
  }

  private def fmtLine(layout: Layout): Doc = layout match {
    case Layout.SingleLine => space
    case Layout.MultiLine  => line
  }

  private def fmtNest(layout: Layout, level: Int, doc: Doc): Doc = layout match {
    case Layout.SingleLine => doc
    case Layout.MultiLine  => Nest(level, doc)
  }

  def format(tree: Tree): String = {
    val doc = traverse(tree)
    Doc.pretty(doc)
  }

  def traverse(tree: Tree): Doc = tree.kind match {
    case TreeKind.Root                          => prettyRoot(tree)
    case TreeKind.Expr.Binary                   => prettyBinary(tree)
    case TreeKind.ParameterList                 => prettyParameterList(tree)
    case TreeKind.Parameter                     => prettyParameter(tree)
    case TreeKind.Decl.Def                      => prettyDef(tree)
    case TreeKind.UsesOrImports.UseOrImportList => prettyUseOrImportList(tree)
    case TreeKind.UsesOrImports.Import          => prettyImport(tree)
    case TreeKind.UsesOrImports.Use             => prettyUse(tree)
    case TreeKind.ArgumentList                  => prettyArgumentList(tree)
    case TreeKind.Expr.Apply                    => prettyApply(tree)
    case TreeKind.Expr.Lambda                   => prettyLambda(tree)
    case TreeKind.Expr.Block                    => prettyBlock(tree)
    case TreeKind.Expr.Statement                => prettyStatement(tree)
    case TreeKind.Expr.LetMatch                 => prettyLetMatch(tree)
    case TreeKind.Expr.NewObject                => prettyNewObject(tree)
    case TreeKind.Expr.InvokeConstructor        => prettyInvokeConstructor(tree)
    case TreeKind.Expr.JvmMethod                => prettyDef(tree)
    case TreeKind.Type.Binary                   => prettyBinary(tree)
    case TreeKind.QName                         => prettyQName(tree)
    case TreeKind.Ident                         => prettyIdent(tree)
    case TreeKind.Operator                      => prettyOperator(tree)
    case TreeKind.Decl.Module                   => prettyModule(tree)
    case TreeKind.Doc                           => prettyDoc(tree)
    case TreeKind.CommentList                   => prettyCommentList(tree)
    case TreeKind.Decl.Enum                     => prettyEnum(tree)
    case TreeKind.Case                          => prettyCase(tree)
    case TreeKind.ModifierList                  => prettyModifierList(tree)
    case TreeKind.Decl.Instance                 => prettyInstance(tree)
    case TreeKind.Decl.Effect                   => prettyEffect(tree)
    case TreeKind.Expr.Match                    => prettyMatch(tree)
    case TreeKind.Decl.TypeAlias                => prettyTypeAlias(tree)
    case _ => prettyFallback(tree)
  }

  private def prettyTypeAlias(tree: Tree): Doc = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val (docChildren, rest) = children.partition {
      case t: Tree if t.kind == TreeKind.Doc => true
      case _ => false
    }

    val docDoc = docChildren.map(prettyChild).reduceLeftOption(_ <|> _).getOrElse(empty)
    val sig = spaceJoinChildren(rest, noSpacePairs = Set.empty)

    if (docChildren.nonEmpty) docDoc <|> sig else sig
  }

  private def prettyMatch(tree: Tree): Doc = {
    val layout = layoutOf(tree)

    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val openIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.CurlyL => true
      case _ => false
    }

    val closeIndex = children.lastIndexWhere {
      case token: Token if token.kind == TokenKind.CurlyR => true
      case _ => false
    }

    if (openIndex < 0 || closeIndex < 0) {
      return spaceJoinChildren(children, Set.empty)
    }

    val header = children.slice(0, openIndex)
    val body   = children.slice(openIndex + 1, closeIndex)

    val headerDoc = spaceJoinChildren(header, Set.empty)

    val bodyParts = body.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val bodyDoc =
      bodyParts
        .map(prettyChild)
        .reduceLeftOption(_ <> line <> _)
        .getOrElse(empty)

    headerDoc <+> text("{") <>
      fmtNest(layout, 4, line <> bodyDoc) <>
      line <> text("}")
  }

  private def prettyEffect(tree: Tree): Doc = {

    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val openIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.CurlyL => true
      case _ => false
    }

    val closeIndex = children.lastIndexWhere {
      case token: Token if token.kind == TokenKind.CurlyR => true
      case _ => false
    }

    if (openIndex < 0 || closeIndex < 0) {
      return spaceJoinChildren(children, Set.empty)
    }

    val header = children.slice(0, openIndex)
    val body   = children.slice(openIndex + 1, closeIndex)

    val headerDoc = spaceJoinChildren(header, Set.empty)

    val bodyParts = body.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val bodyDoc =
      bodyParts
        .map(prettyChild)
        .reduceLeftOption(_ <> line <> _)
        .getOrElse(empty)

    headerDoc <+> text("{") <>
      Nest(4, line <> bodyDoc) <>
      line <> text("}")
  }

  private def prettyModifierList(tree: Tree): Doc = {
    val mods = tree.children.map(prettyChild)
    mods.reduceLeftOption(_ <+> _).getOrElse(empty)
  }

  private def prettyInstance(tree: Tree): Doc = {
    val layout = layoutOf(tree)

    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val openIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.CurlyL => true
      case _ => false
    }

    val closeIndex = children.lastIndexWhere {
      case token: Token if token.kind == TokenKind.CurlyR => true
      case _ => false
    }

    if (openIndex < 0 || closeIndex < 0) {
      return spaceJoinChildren(
        children,
        noSpacePairs = Set.empty
      )
    }

    val header = children.slice(0, openIndex)
    val body   = children.slice(openIndex + 1, closeIndex)

    val headerDoc = spaceJoinChildren(
      header,
      noSpacePairs = Set((TreeKind.Ident, TreeKind.TypeParameterList))
    )

    val bodyParts = body.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val bodyDoc =
      bodyParts
        .map(prettyChild)
        .reduceLeftOption(_ <> line <> _)
        .getOrElse(empty)

    headerDoc <+> text("{") <>
      fmtNest(layout, 4, line <> bodyDoc) <>
      line <>
      text("}")
  }

  private def prettyCase(tree: Tree): Doc = {
    val parts = tree.children

    val groups = parts.foldLeft(List.empty[List[SyntaxTree.Child]]) {
      case (Nil, child) =>
        List(List(child))

      case (acc@(_ :: _), token: Token)
        if token.kind == TokenKind.KeywordCase =>
        List(token) :: acc

      case (current :: rest, child) =>
        (child :: current) :: rest
    }.map(_.reverse).reverse

    val caseDocs = groups.map { group =>
      spaceJoinChildren(
        group.toArray,
        noSpacePairs = Set((TreeKind.Ident, TreeKind.ParameterList))
      )
    }

    caseDocs.reduceLeftOption(_ <> line <> _).getOrElse(empty)
  }


  /**
    * Formats a documentation comment, ensuring that each line of the comment is on its own line.
    *
    * @param tree The documentation comment tree.
    * @return A Doc representing the formatted documentation comment.
    */
  private def prettyDoc(tree: Tree): Doc = {
    val parts = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }.map(prettyChild)
    if (parts.isEmpty) return empty
    parts.reduceLeft(_ <|> _)
  }

  /**
    * Formats a list of comments, ensuring that each comment is on its own line.
    * For example, a list of comments like "// Comment 1\n// Comment 2" would be formatted as:
    * // Comment 1
    * // Comment 2
    * TODO: Currently, it adds a second line break before a function? We should try to figure out why.
    * @param tree
    * @return
    */
  private def prettyCommentList(tree: Tree): Doc = {
    val comments = tree.children.collect { case token: Token => text(token.text) }
    if (comments.isEmpty) return empty
    line <> comments.reduceLeft(_ <|> _)
  }

  /**
    * Formats an enum declaration, ensuring that the opening curly brace is on the same line as the code before.
    * The body of the enum is indented by 4 spaces and the closing curly brace.
    * TODO: Multiple ways to format an enum declaration cause ambigious formatting.
    * @param tree The enum declaration tree
    * @return A Doc representing the formatted enum declaration.
    */
  private def prettyEnum(tree: Tree): Doc = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val openIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.CurlyL => true
      case _ => false
    }

    val closeIndex = children.lastIndexWhere {
      case token: Token if token.kind == TokenKind.CurlyR => true
      case _ => false
    }

    if (openIndex < 0 || closeIndex < 0) return prettyFallback(tree)

    val header = children.slice(0, openIndex)
    val body   = children.slice(openIndex + 1, closeIndex)

    val headerDoc = spaceJoinChildren(
      header,
      noSpacePairs = Set((TreeKind.Ident, TreeKind.TypeParameterList)),
      noSpaceBefore = Set(TokenKind.Colon)
    )

    val bodyDoc =
      body.map(prettyChild).reduceLeftOption(_ <> line <> _).getOrElse(empty)

    headerDoc <+> text("{") <>
      Nest(4, line <> bodyDoc) <>
      line <> text("}")
  }

  /**
    * Formats a module declaration, ensuring that the opening curly brace is on the same line as the code before.
    * The body of the module is indented by 4 spaces and the closing curly brace is on its own line.
    * For example, a module declaration like "module MyModule { ... }" would be formatted as:
    * module MyModule {
    *   <body>
    * }
    * @param treen The module declaration tree.
    * @return A Doc representing the formatted module declaration.
    */
  private def prettyModule(tree: Tree): Doc = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val openIndex = children.indexWhere {
      case token: Token if token.kind == TokenKind.CurlyL => true
      case _ => false
    }
    val closeIndex = children.lastIndexWhere {
      case token: Token if token.kind == TokenKind.CurlyR => true
      case _ => false
    }

    if (openIndex < 0 || closeIndex < 0) return prettyFallback(tree)

    val header = children.slice(0, openIndex)
    val body = children.slice(openIndex + 1, closeIndex)
    val layout = layoutOf(tree)

    val headerDoc = header.map(prettyChild).reduceLeftOption(_ <+> _).getOrElse(empty)

    val bodyParts = body.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }
    val bodyDoc = bodyParts.map(prettyChild).reduceLeftOption(_ <> line <> line <> _).getOrElse(empty)

    headerDoc <+> text("{") <>
      fmtNest(layout, 4, line <> bodyDoc) <>
      line <> text("}")
  }

  /**
    * Formats an identifier, ensuring that it is not concatenated with adjacent tokens without a space.
    *
    * @param tree The identifier tree.
    * @return A Doc representing the formatted identifier.
    */
  private def prettyIdent(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  /**
    * Formats an operator, ensuring that it is not concatenated with adjacent tokens without a space.
    * For example, an operator like "+" would be formatted as " + " when it appears between two expressions.
    *
    * @param tree The operator tree.
    * @return A Doc representing the formatted operator.
    */
  private def prettyOperator(tree: Tree): Doc =
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

  /**
    * Formats a qualified name, ensuring that it is not concatenated with adjacent tokens without a space.
    *
    * @param tree The qualified name tree.
    * @return A Doc representing the formatted qualified name.
    */
  private def prettyQName(tree: Tree): Doc = {
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
  }

  /**
    * Formats a new object expression, ensuring that the `new` keyword is followed by a space and that the body of the new object is properly indented.
    *
    * @param tree The new object expression.
    * @return A Doc representing the formatted new object expression.
    */
  private def prettyNewObject(tree: Tree): Doc = {
    val layout = layoutOf(tree)

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

    prefix <> space <>
      text("{") <>
      fmtNest(layout, 4, fmtLine(layout) <> inner) <>
      fmtLine(layout) <>
      text("}") <>
      suffix
  }

  /**
    * Formats an invoke constructor expression, ensuring that the `new` keyword is followed by a space.
    * For example, an invoke constructor expression would be formatted as "new MyClass()".
    *
    * @param tree The invoke constructor expression tree.
    * @return A Doc representing the formatted invoke constructor expression, with proper spacing after the `new` keyword.
    */
  private def prettyInvokeConstructor(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.KeywordNew =>
        acc <> text("new") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formats a statement, ensuring that semicolons are followed by line breaks.
    * For example, a statement like "val x = 10; val y = 20" would be formatted as:
    * val x = 10;
    * val y = 20
    *
    * @param tree The statement tree, expected to have the form
    * @return A Doc representing the formatted statement, with proper spacing and line breaks after semicolons.
    */
  private def prettyStatement(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Semi =>
        acc <> text(";") <> line
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formats an argument list, ensuring that commas are followed by spaces
    * For example, an argument list like "(x,y)" would be formatted as "(x, y)".
    *
    * @param tree The argument list tree.
    * @return A Doc representing the formatted argument list, with proper spacing after commas.
    */
  private def prettyArgumentList(tree: Tree): Doc = {
    val hasArgs = tree.children.exists {
      case t: Tree => t.kind == TreeKind.Argument || t.kind == TreeKind.ArgumentNamed
      case _ => false
    }
    if (!hasArgs) {
      return text("(") <> text(")")
    }
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Comma =>
        acc <> text(",") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formats an apply expression, ensuring that the `new` keyword is followed by a space.
    * For example, an apply expression would be formatted as "new MyClass()".
    *
    * @param tree The apply expression tree, expected to have the form.
    * @return A Doc representing the formatted apply expression, with proper spacing after the `new` keyword.
    */
  private def prettyApply(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.KeywordNew =>
        acc <> text("new") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formats a lambda expression, ensuring that the `->` operator is surrounded by spaces.
    * For example, a lambda like "x->x+1" would be formatted as "x -> x + 1".
    *
    * @param tree The lambda expression tree.
    * @return A Doc representing the formatted lambda expression, with proper spacing around the `->` operator.
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
    * Formats a let-match expression, ensuring that the `let` keyword is followed by a space, the `=`
    * operator is surrounded by spaces, and each statement is separated by a line break.
    *
    * @param tree The let-match expression tree
    * @return  A Doc representing the formatted let-match expression, with proper spacing and line breaks.
    */
  private def prettyLetMatch(tree: Tree): Doc = {
    val layout = layoutOf(tree)
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.KeywordLet =>
        acc <> text("let") <> space
      case (acc, token: Token) if token.kind == TokenKind.Equal =>
        acc <> space <> text("=") <> space
      case (acc, token: Token) if token.kind == TokenKind.Semi =>
        acc <> text(";") <> fmtLine(layout)
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formats a block expression, ensuring that the opening curly brace is on the same line as the code before.
    * The body of the block is indented by 4 spaces and the closing curly brace is on its own line.
    * For example, a block like "{ val x = 10; x + 1 }" would be formatted as:
    * {
    *    val x = 10;
    *    x + 1
    * }
    *
    * @param tree The block expression tree, expected to have the form
    * @return A Doc representing the formatted block expression, with proper indentation and line breaks.
    */
  private def prettyBlock(tree: Tree): Doc = {
    val layout = layoutOf(tree)
    val body = tree.children.filter {
      case token: Token if token.kind == TokenKind.CurlyL => false
      case token: Token if token.kind == TokenKind.CurlyR => false
      case _ => true
    }.map(prettyChild)

    val inner = body.reduceLeftOption(_ <> _).getOrElse(empty)

    text("{") <>
      fmtNest(layout, 4, fmtLine(layout) <> inner) <>
      fmtLine(layout) <>
      text("}")
  }

  /**
    * Formats a list of use or import statements, ensuring that each statement is on its own line.
    *
    * @param tree The tree representing the list of use or import statements, expected to have the form: [UseOrImportList, (Use|Import)*]
    * @return A Doc representing the formatted list of use or import statements, with each statement on its own line
    */
  private def prettyUseOrImportList(tree: Tree): Doc = {
    val children = tree.children.collect { case t: Tree => traverse(t) }
    if (children.isEmpty) return empty
    children.reduceLeft(_ <|> _)
  }

  /**
    * Formats an import statement, ensuring that the `import` keyword is followed by a space.
    *
    * @param tree The import statement tree, expected to have the form: [KeywordImport, QName, (Comma, QName)*]
    * @return A Doc representing the formatted import statement, e.g., "import java.awt.event.ActionEvent, java.util.List"
    */
  private def prettyImport(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.KeywordImport =>
        acc <> text("import") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formatting a use statement, ensuring that the `use` keyword is followed by a space.
    *
    * @param tree The use statement tree, expected to have the form: [KeywordUse, QName, (Comma, QName)*]
    * @return A Doc representing the formatted use statement, e.g., "use java.awt.event.ActionEvent, java.util.List"
    */
  private def prettyUse(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.KeywordUse =>
        acc <> text("use") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  /**
    * Formats the root of the syntax tree by concatenating its non-empty children.
    * Empty children (i.e., those with no children themselves) are filtered out to avoid unnecessary whitespace.
    *
    * @param tree The root tree to format, expected to have multiple children representing top-level declarations or expressions.
    * @return A Doc representing the formatted root, which is a concatenation of its non-empty children, separated by line breaks if necessary.
    */
  private def prettyRoot(tree: Tree): Doc = {
    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }
    if (children.isEmpty) return empty
    children.map(prettyChild).reduceLeft(_ <|> empty <|> _)
  }

  private def prettyDef(tree: Tree): Doc = {
    val layout = layoutOf(tree)

    val children = tree.children.filter {
      case t: Tree if t.children.isEmpty => false
      case _ => true
    }

    val (docChildren, rest) = children.partition {
      case t: Tree if t.kind == TreeKind.Doc => true
      case _ => false
    }

    val docDoc =
      docChildren.map(prettyChild).reduceLeftOption(_ <|> _).getOrElse(empty)

    val eqIndex = rest.indexWhere {
      case token: Token if token.kind == TokenKind.Equal => true
      case _ => false
    }

    if (eqIndex < 0) {
      val sig = spaceJoinChildren(
        rest,
        noSpacePairs = Set((TreeKind.Ident, TreeKind.ParameterList)),
        noSpaceBefore = Set(TokenKind.Colon)
      )

      return if (docChildren.nonEmpty) docDoc <|> sig else sig
    }

    val sigParts  = rest.take(eqIndex)
    val bodyParts = rest.drop(eqIndex + 1)
    val sig = spaceJoinChildren(
      sigParts,
      noSpacePairs = Set((TreeKind.Ident, TreeKind.ParameterList)),
      noSpaceBefore = Set(TokenKind.Colon)
    )

    val body =
      bodyParts.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)

    val bodyIsBlock = bodyParts.exists {
      case t: Tree =>
        t.kind == TreeKind.Expr.Block ||
          t.children.exists {
            case inner: Tree => inner.kind == TreeKind.Expr.Block
            case _ => false
          }
      case _ => false
    }

    val defDoc =
      if (bodyIsBlock) {
        sig <+> text("=") <+> body
      } else {
        sig <+> text("=") <> fmtNest(layout, 4, fmtLine(layout) <> body)
      }

    if (docChildren.nonEmpty) docDoc <|> defDoc else defDoc
  }

  /**
    * Formats a binary expression, ensuring that operators are surrounded by spaces.
    * For example, an expression like "a+b" would be formatted as "a + b".
    *
    * @param tree The binary expression tree, expected to have the form: [Expr, Operator, Expr]
    * @return A Doc representing the formatted binary expression, e.g., "a + b"
    */
  private def prettyBinary(tree: Tree): Doc = {
    val parts = tree.children.map(prettyChild)
    parts.reduceLeftOption(_ <+> _).getOrElse(empty)
  }

  /**
    * Formats a parameter list, ensuring that commas are followed by spaces.
    * For example, a parameter list like "(x: Int,y: String)" would be formatted as "(x: Int, y: String)".
    *
    * @param tree The parameter list tree, expected to have the form: [OpenParen, Parameter, (Comma, Parameter)*, CloseParen]
    * @return A Doc representing the formatted parameter list, e.g., "(x: Int, y: String)"
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
    * Formats a parameter, ensuring that the colon is followed by a space.
    * For example, a parameter like "x:Int" would be formatted as "x: Int".
    *
    * @param tree The parameter tree, expected to have the form: [Ident, Colon, Type]
    * @return A Doc representing the formatted parameter, e.g., "x: Int"
    */
  private def prettyParameter(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Colon =>
        acc <> text(":") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  private def prettyChild(child: SyntaxTree.Child): Doc = child match {
    case token: Token => text(token.text)
    case tree: Tree   => traverse(tree)
  }

  private def prettyFallback(tree: Tree): Doc = {
    val children = tree.children
    if (children.isEmpty) return empty

    children.sliding(2).foldLeft(prettyChild(children.head): Doc) {
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

  private def leftMostToken(child: SyntaxTree.Child): Option[Token] = child match {
    case token: Token => Some(token)
    case tree: Tree   => tree.children.headOption.flatMap(leftMostToken)
  }

  private def rightMostToken(child: SyntaxTree.Child): Option[Token] = child match {
    case token: Token => Some(token)
    case tree: Tree   => tree.children.lastOption.flatMap(rightMostToken)
  }

  /**
    * Joins non-empty children with spaces, except between specific
    * adjacent TreeKind pairs.
    * For example, in a function definition, we don't want a space between the function name and its parameter list.
    * @param children The array of children to join.
    * @param noSpacePairs A set of TreeKind pairs for which no space should be inserted between them.
    * @return A Doc representing the joined children with appropriate spacing.
    */
  private def spaceJoinChildren(
    children: Array[SyntaxTree.Child],
    noSpacePairs: Set[(TreeKind, TreeKind)],
    noSpaceBefore: Set[TokenKind] = Set.empty
  ): Doc = {
    if (children.isEmpty) return empty

    children.sliding(2).foldLeft(prettyChild(children.head): Doc) {
      case (acc, Array(prev, next)) =>
        val noSpace = (prev, next) match {
          case (p: Tree, n: Tree) if noSpacePairs.contains((p.kind, n.kind)) =>
            true
          case (_, token: Token) if noSpaceBefore.contains(token.kind) =>
            true
          case _ =>
            false
        }

        val sep = if (noSpace) empty else space
        acc <> sep <> prettyChild(next)

      case (acc, _) => acc
    }
  }
}
