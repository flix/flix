package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import scala.collection.immutable.Seq;
import collection.JavaConversions._;
import org.antlr.v4.runtime.Token;

class AST_FlixVisitor(source: SourceInput, input: String) /*extends FlixVisitor[Object]*/{


	//SOURCE POSITION NODE
	def visitSp(tk: Token): SourcePosition = SourcePosition(
			source,
			tk.getLine(),
			tk.getCharPositionInLine(),
			Some(input)
		)

	//ROOT NODE
	def visitStart(ctx: FlixParser.StartContext): ParsedAst.Root = ParsedAst.Root(
			ctx.s_import().map(visitS_import).toList.toSeq,
			ctx.decl().map(visitDecl).toList.toSeq
		)

	//NAME RULES
	def visitIdent(ctx: FlixParser.IdentContext) =  Name.Ident(
			visitSp(ctx.getStart()),
			ctx.getChild(0).getText(),
			visitSp(ctx.getStop())
		)

	def visitNname(ctx: FlixParser.NnameContext) = Name.NName(
			visitSp(ctx.getStart()),
			ctx.ident().map(visitIdent).toList,
			visitSp(ctx.getStop())
		)

	//IMPORT RULES
	def visitS_import(ctx: FlixParser.S_importContext): ParsedAst.Import =  ctx.getChild(ctx.getChildCount-1) match {
			case x: FlixParser.Import_wildcardContext => visitImport_wildcard(x)
			case x: FlixParser.Import_definitionContext => visitImport_definition(x)
			case x: FlixParser.Import_namespaceContext =>  visitImport_namespace(x)
		}

	def visitImport_wildcard(ctx: FlixParser.Import_wildcardContext) = ParsedAst.Import.Wild(
			visitSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitSp(ctx.getStop())
		)

	def visitImport_definition(ctx: FlixParser.Import_definitionContext) = ParsedAst.Import.Definition(
			visitSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitIdent(ctx.ident()),
			visitSp(ctx.getStop())
		)

	def visitImport_namespace(ctx: FlixParser.Import_namespaceContext) =ParsedAst.Import.Namespace(
			visitSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitSp(ctx.getStop())
		)


	//DECLARATION RULES
	def visitDecl(ctx: FlixParser.DeclContext): ParsedAst.Declaration = ctx.getChild(0) match {
			case x: FlixParser.Decls_namespaceContext => visitDecls_namespace(x)
			case x: FlixParser.Decls_enumContext => visitDecls_enum(x)
			case x: FlixParser.Decls_relationContext => visitDecls_relation(x)
			case x: FlixParser.Decls_latticeContext => visitDecls_lattice(x)
			case x: FlixParser.Decls_indexContext => visitDecls_index(x)
			case x: FlixParser.Decls_signatureContext => visitDecls_signature(x)
			case x: FlixParser.Decls_externalContext => visitDecls_external(x)
			case x: FlixParser.Decls_definitionContext => visitDecls_definition(x)
			case x: FlixParser.Decls_lawContext => visitDecls_law(x)
			case x: FlixParser.Decls_classContext => visitDecls_class(x)
			case x: FlixParser.Decls_factContext => visitDecls_fact(x)
			case x: FlixParser.Decls_ruleContext => visitDecls_rule(x)
			case x: FlixParser.Decls_letlatticeContext => visitDecls_letlattice(x)
		}

	def visitDecls_namespace(FlixParser.Decls_namespaceContext ctx) = ParsedAst.Declaration.Namespace(
			visitSp(ctx.getStart()),
			visitNname(ctx.nname()),
			ctx.decl().map(visitDecl).toList.toSeq
			visitSp(ctx.getStop())
		)
	
	def visitDecls_enum(FlixParser.Decls_enumContext ctx);
	
	def visitDcases(FlixParser.DcasesContext ctx);
	
	def visitDcase(FlixParser.DcaseContext ctx);
	
	def visitDecls_relation(FlixParser.Decls_relationContext ctx);
	
	def visitDecls_lattice(FlixParser.Decls_latticeContext ctx);
	
	def visitDecls_index(FlixParser.Decls_indexContext ctx);
	
	def visitDecls_signature(FlixParser.Decls_signatureContext ctx);
	
	def visitDecls_external(FlixParser.Decls_externalContext ctx);
	
	def visitDecls_definition(FlixParser.Decls_definitionContext ctx);
	
	def visitDecls_law(FlixParser.Decls_lawContext ctx);
	
	def visitDecls_class(FlixParser.Decls_classContext ctx);
	
	def visitClass_body(FlixParser.Class_bodyContext ctx);
	
	def visitDecls_fact(FlixParser.Decls_factContext ctx);
	
	def visitDecls_rule(FlixParser.Decls_ruleContext ctx);
	
	def visitElms(FlixParser.ElmsContext ctx);
	
	def visitDecls_letlattice(FlixParser.Decls_letlatticeContext ctx);
	
	def visitDecls_impl(FlixParser.Decls_implContext ctx);