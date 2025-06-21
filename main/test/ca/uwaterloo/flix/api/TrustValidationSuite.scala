package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext}
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Symbol, TypedAst}
import ca.uwaterloo.flix.tools.pkg.Permissions
import ca.uwaterloo.flix.tools.pkg.{Dependency, Repository, SemVer}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class TrustValidationSuite extends AnyFunSuite with TestUtils {

  private val TestLibName = "testLib"

  test("TrustValidation.01") {
    val input =
      """
        |pub def noEff(): Int32 = 2
        |""".stripMargin
    val (root, flix) = checkLib(input, "noEFf", Options.TestWithLibNix)
    val dep = mkDependency(TestLibName, Permissions.PlainFlix)
    val result = flix.validateTrust(root, Set(dep))
    assert(result.isEmpty)
  }

  ignore("TrustValidation.02") {
    val input =
      """
        |pub def f(): Int32 = unchecked_cast(2 as Int32 \ {})
        |""".stripMargin
    val (root, flix) = checkLib(input, "f", Options.TestWithLibNix)
    val dep = mkDependency(TestLibName, Permissions.PlainFlix)
    val result = flix.validateTrust(root, Set(dep))
    assert(result.nonEmpty)
  }

  private def mkDependency(name: String, perm: Permissions): Dependency.FlixDependency = {
    Dependency.FlixDependency(Repository.GitHub, "", name, SemVer(0, 1, 0), perm)
  }

  private def checkLib(input: String, lib: String, o: Options): (TypedAst.Root, Flix) = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    val flix = new Flix().setOptions(o).addSourceCode("<test>", input)
    val (Some(root), _) = flix.check()
    val withLibs = root.defs.map {
      case (_, TypedAst.Def(sym, spec, exp, defLoc)) if sym.text == lib =>
        val symLoc1 = toLibLoc(sym.loc)
        val sym1 = updateSymLoc(sym, symLoc1)
        val exp1 = updateExprLoc(exp)
        val defLoc1 = toLibLoc(defLoc)
        sym -> TypedAst.Def(sym1, spec, exp1, defLoc1)
      case sd => sd
    }
    (root.copy(defs = withLibs), flix)
  }

  private def updateSymLoc(sym: Symbol.DefnSym, loc: SourceLocation): Symbol.DefnSym = {
    new Symbol.DefnSym(sym.id, sym.namespace, sym.text, loc)
  }

  private def toLibLoc(loc: SourceLocation): SourceLocation = {
    val (sp1, sp2) = toLibSp(loc.sp1, loc.sp2)
    loc.copy(sp1 = sp1, sp2 = sp2)
  }

  private def toLibSp(sp1: SourcePosition, sp2: SourcePosition): (SourcePosition, SourcePosition) = {
    val input = toLibInput(sp1.source.input)
    val source = sp1.source.copy(input = input)
    (sp1.copy(source), sp2.copy(source))
  }

  private def toLibInput(input: Input): Input = input match {
    case Input.Text(_, text, sctx) => Input.FileInPackage(Path.of(TestLibName), "", text, sctx)
    case Input.TxtFile(_, _) => ???
    case Input.PkgFile(_, _) => ???
    case Input.FileInPackage(_, _, _, _) => input
    case Input.Unknown => ???
  }

  private def updateExprLoc(exp0: TypedAst.Expr): TypedAst.Expr = exp0 match {
    case Expr.Cst(cst, tpe, loc) => Expr.Cst(cst, tpe, toLibLoc(loc))
    case Expr.Var(sym, tpe, loc) => Expr.Var(sym, tpe, toLibLoc(loc))
    case Expr.Hole(sym, scp, tpe, eff, loc) => Expr.Hole(sym, scp, tpe, eff, toLibLoc(loc))
    case Expr.HoleWithExp(exp, scp, tpe, eff, loc) => Expr.HoleWithExp(updateExprLoc(exp), scp, tpe, eff, toLibLoc(loc))
    case Expr.OpenAs(symUse, exp, tpe, loc) => Expr.OpenAs(symUse, updateExprLoc(exp), tpe, toLibLoc(loc))
    case Expr.Use(sym, alias, exp, loc) => Expr.Use(sym, alias, updateExprLoc(exp), toLibLoc(loc))
    case Expr.Lambda(fparam, exp, tpe, loc) => Expr.Lambda(fparam, updateExprLoc(exp), tpe, toLibLoc(loc))
    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) => Expr.ApplyClo(updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.ApplyDef(symUse, exps, itpe, tpe, eff, loc) => Expr.ApplyDef(symUse, exps.map(updateExprLoc), itpe, tpe, eff, toLibLoc(loc))
    case Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, loc) => Expr.ApplyLocalDef(symUse, exps.map(updateExprLoc), arrowTpe, tpe, eff, toLibLoc(loc))
    case Expr.ApplySig(symUse, exps, itpe, tpe, eff, loc) => Expr.ApplySig(symUse, exps.map(updateExprLoc), itpe, tpe, eff, toLibLoc(loc))
    case Expr.Unary(sop, exp, tpe, eff, loc) => Expr.Unary(sop, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => Expr.Binary(sop, updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.Let(bnd, exp1, exp2, tpe, eff, loc) => Expr.Let(bnd, updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, loc) => Expr.LocalDef(bnd, fparams, updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.Region(tpe, loc) => Expr.Region(tpe, toLibLoc(loc))
    case Expr.Scope(bnd, regSym, exp, tpe, eff, loc) => Expr.Scope(bnd, regSym, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => Expr.IfThenElse(updateExprLoc(exp1), updateExprLoc(exp2), updateExprLoc(exp3), tpe, eff, toLibLoc(loc))
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => Expr.Stm(updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.Discard(exp, eff, loc) => Expr.Discard(updateExprLoc(exp), eff, toLibLoc(loc))
    case Expr.Match(exp, rules, tpe, eff, loc) => Expr.Match(updateExprLoc(exp), rules, tpe, eff, toLibLoc(loc))
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) => Expr.TypeMatch(updateExprLoc(exp), rules, tpe, eff, toLibLoc(loc))
    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => Expr.RestrictableChoose(star, updateExprLoc(exp), rules, tpe, eff, toLibLoc(loc))
    case Expr.ExtensibleMatch(label, exp1, bnd1, exp2, bnd2, exp3, tpe, eff, loc) => Expr.ExtensibleMatch(label, updateExprLoc(exp1), bnd1, updateExprLoc(exp2), bnd2, updateExprLoc(exp3), tpe, eff, toLibLoc(loc))
    case Expr.Tag(sym, exps, tpe, eff, loc) => Expr.Tag(sym, exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.RestrictableTag(sym, exps, tpe, eff, loc) => Expr.RestrictableTag(sym, exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.ExtensibleTag(label, exps, tpe, eff, loc) => Expr.ExtensibleTag(label, exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.Tuple(exps, tpe, eff, loc) => Expr.Tuple(exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.RecordSelect(exp, label, tpe, eff, loc) => Expr.RecordSelect(updateExprLoc(exp), label, tpe, eff, toLibLoc(loc))
    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => Expr.RecordExtend(label, updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.RecordRestrict(label, exp, tpe, eff, loc) => Expr.RecordRestrict(label, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.ArrayLit(exps, exp, tpe, eff, loc) => Expr.ArrayLit(exps.map(updateExprLoc), updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => Expr.ArrayNew(updateExprLoc(exp1), updateExprLoc(exp2), updateExprLoc(exp3), tpe, eff, toLibLoc(loc))
    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => Expr.ArrayLoad(updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.ArrayLength(exp, eff, loc) => Expr.ArrayLength(updateExprLoc(exp), eff, toLibLoc(loc))
    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => Expr.ArrayStore(updateExprLoc(exp1), updateExprLoc(exp2), updateExprLoc(exp3), eff, toLibLoc(loc))
    case Expr.StructNew(sym, fields, region, tpe, eff, loc) => Expr.StructNew(sym, fields, region, tpe, eff, toLibLoc(loc))
    case Expr.StructGet(exp, sym, tpe, eff, loc) => Expr.StructGet(updateExprLoc(exp), sym, tpe, eff, toLibLoc(loc))
    case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) => Expr.StructPut(updateExprLoc(exp1), sym, updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.VectorLit(exps, tpe, eff, loc) => Expr.VectorLit(exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => Expr.VectorLoad(updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.VectorLength(exp, loc) => Expr.VectorLength(updateExprLoc(exp), toLibLoc(loc))
    case Expr.Ascribe(exp, expectedType, expectedEff, tpe, eff, loc) => Expr.Ascribe(updateExprLoc(exp), expectedType, expectedEff, tpe, eff, toLibLoc(loc))
    case Expr.InstanceOf(exp, clazz, loc) => Expr.InstanceOf(updateExprLoc(exp), clazz, toLibLoc(loc))
    case Expr.CheckedCast(cast, exp, tpe, eff, loc) => Expr.CheckedCast(cast, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => Expr.UncheckedCast(updateExprLoc(exp), declaredType, declaredEff, tpe, eff, toLibLoc(loc))
    case Expr.Unsafe(exp, runEff, tpe, eff, loc) => Expr.Unsafe(updateExprLoc(exp), runEff, tpe, eff, toLibLoc(loc))
    case Expr.Without(exp, sym, tpe, eff, loc) => Expr.Without(updateExprLoc(exp), sym, tpe, eff, toLibLoc(loc))
    case Expr.TryCatch(exp, rules, tpe, eff, loc) => Expr.TryCatch(updateExprLoc(exp), rules, tpe, eff, toLibLoc(loc))
    case Expr.Throw(exp, tpe, eff, loc) => Expr.Throw(updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.Handler(sym, rules, bodyType, bodyEff, handledEff, tpe, loc) => Expr.Handler(sym, rules, bodyType, bodyEff, handledEff, tpe, toLibLoc(loc))
    case Expr.RunWith(exp1, exp2, tpe, eff, loc) => Expr.RunWith(updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.Do(op, exps, tpe, eff, loc) => Expr.Do(op, exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => Expr.InvokeConstructor(constructor, exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => Expr.InvokeMethod(method, updateExprLoc(exp), exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => Expr.InvokeStaticMethod(method, exps.map(updateExprLoc), tpe, eff, toLibLoc(loc))
    case Expr.GetField(field, exp, tpe, eff, loc) => Expr.GetField(field, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => Expr.PutField(field, updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.GetStaticField(field, tpe, eff, loc) => Expr.GetStaticField(field, tpe, eff, toLibLoc(loc))
    case Expr.PutStaticField(field, exp, tpe, eff, loc) => Expr.PutStaticField(field, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => Expr.NewObject(name, clazz, tpe, eff, methods, toLibLoc(loc))
    case Expr.NewChannel(exp, tpe, eff, loc) => Expr.NewChannel(updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.GetChannel(exp, tpe, eff, loc) => Expr.GetChannel(updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => Expr.PutChannel(updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.SelectChannel(rules, default, tpe, eff, loc) => Expr.SelectChannel(rules, default, tpe, eff, toLibLoc(loc))
    case Expr.Spawn(exp1, exp2, tpe, eff, loc) => Expr.Spawn(updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.ParYield(frags, exp, tpe, eff, loc) => Expr.ParYield(frags, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.Lazy(exp, tpe, loc) => Expr.Lazy(updateExprLoc(exp), tpe, toLibLoc(loc))
    case Expr.Force(exp, tpe, eff, loc) => Expr.Force(updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.FixpointConstraintSet(cs, tpe, loc) => Expr.FixpointConstraintSet(cs, tpe, toLibLoc(loc))
    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => Expr.FixpointLambda(pparams, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => Expr.FixpointMerge(updateExprLoc(exp1), updateExprLoc(exp2), tpe, eff, toLibLoc(loc))
    case Expr.FixpointSolve(exp, tpe, eff, loc) => Expr.FixpointSolve(updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => Expr.FixpointFilter(pred, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.FixpointInject(exp, pred, tpe, eff, loc) => Expr.FixpointInject(updateExprLoc(exp), pred, tpe, eff, toLibLoc(loc))
    case Expr.FixpointProject(pred, exp, tpe, eff, loc) => Expr.FixpointProject(pred, updateExprLoc(exp), tpe, eff, toLibLoc(loc))
    case Expr.Error(m, tpe, eff) => Expr.Error(m, tpe, eff)
  }
}
