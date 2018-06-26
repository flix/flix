package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, ExecutableAst, Symbol}
import ca.uwaterloo.flix.runtime.{InvocationTarget, Linker}
import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

object ApiBridge {

  // TODO: Thread Flix object through
  // TODO: Name of parameters
  // TODO: public vs. private.
  // TODO: Transitory class.
  // TODO: Accept arguments only as proxy objects?

  def translate(root: ExecutableAst.Root)(implicit flix: Flix): ConstraintSystem = {
    implicit val _ = root

    val strata = root.strata.map(visitStratum)
    val tables = visitTables(root.tables)
    val latOps = visitLatOps(root.tables)
    ConstraintSystem(strata, tables, latOps)
  }

  def visitStratum(stratum: ExecutableAst.Stratum)(implicit root: ExecutableAst.Root, flix: Flix): Stratum = {
    Stratum(stratum.constraints.map(visitConstraint))
  }

  def visitTables(tables: Map[Symbol.TableSym, ExecutableAst.Table])(implicit root: ExecutableAst.Root, flix: Flix): Map[TableSym, Table] = {
    tables.foldLeft(Map.empty[TableSym, Table]) {
      case (macc, (sym, ExecutableAst.Table.Relation(_, attributes, _))) =>
        macc + (visitTableSym(sym) -> Table.Relation(visitTableSym(sym), attributes.map(visitAttribute)))

      case (macc, (sym, ExecutableAst.Table.Lattice(_, keys, value, _))) =>
        macc + (visitTableSym(sym) -> Table.Lattice(visitTableSym(sym), keys.map(visitAttribute), visitAttribute(value)))
    }
  }

  def visitConstraint(c: ExecutableAst.Constraint)(implicit root: ExecutableAst.Root, flix: Flix): Constraint = {
    val cparams = c.cparams.map(visitConstraintParam)
    val head = visitHeadPredicate(c.head)
    val body = c.body.map(visitBodyPredicate)
    Constraint(cparams, head, body)
  }

  def visitConstraintParam(c: ExecutableAst.ConstraintParam)(implicit root: ExecutableAst.Root, flix: Flix): ConstraintParam = c match {
    case ExecutableAst.ConstraintParam.HeadParam(sym, _, _) => ConstraintParam.HeadParam(visitVarSym(sym))
    case ExecutableAst.ConstraintParam.RuleParam(sym, _, _) => ConstraintParam.RuleParam(visitVarSym(sym))
  }

  def visitHeadPredicate(h: ExecutableAst.Predicate.Head)(implicit root: ExecutableAst.Root, flix: Flix): Predicate.Head = h match {
    case ExecutableAst.Predicate.Head.True(_) => Predicate.Head.True()
    case ExecutableAst.Predicate.Head.False(_) => Predicate.Head.False()
    case ExecutableAst.Predicate.Head.Atom(sym, terms, _) => Predicate.Head.Atom(visitTableSym(sym), terms.map(visitHeadTerm))
  }

  def visitBodyPredicate(b: ExecutableAst.Predicate.Body)(implicit root: ExecutableAst.Root, flix: Flix): Predicate.Body = b match {
    case ExecutableAst.Predicate.Body.Atom(sym, polarity, terms, index2sym, loc) =>
      val s = visitTableSym(sym)
      val p = polarity match {
        case Ast.Polarity.Positive => Polarity.Positive
        case Ast.Polarity.Negative => Polarity.Negative
      }
      val ts = terms.map(visitBodyTerm)
      val i2s = index2sym map {
        case x if x != null => visitVarSym(x)
        case _ => null
      }
      Predicate.Body.Atom(s, p, ts, i2s)

    case ExecutableAst.Predicate.Body.Filter(sym, terms, _) =>
      val f = (as: Array[AnyRef]) => Linker.link(sym, root).invoke(as).getValue.asInstanceOf[Boolean].booleanValue()
      val ts = terms.map(visitBodyTerm)
      Predicate.Body.Filter(f, ts)

    case ExecutableAst.Predicate.Body.Loop(_, _, _) => throw new UnsupportedOperationException("Loop currently not supported")
  }

  def visitTableSym(sym: Symbol.TableSym)(implicit root: ExecutableAst.Root, flix: Flix): TableSym = TableSym(sym.toString)

  def visitHeadTerm(t: ExecutableAst.Term.Head)(implicit root: ExecutableAst.Root, flix: Flix): Term.Head = t match {
    case ExecutableAst.Term.Head.Var(sym, _, _) => Term.Head.Var(visitVarSym(sym))
    case ExecutableAst.Term.Head.Lit(lit, _, _) => Term.Head.Lit(() => lit)
    case ExecutableAst.Term.Head.Cst(sym, _, _) => Term.Head.Cst(() => Linker.link(sym, root).invoke(Array.emptyObjectArray))
    case ExecutableAst.Term.Head.App(sym, args, _, _) =>
      val f = (args: Array[AnyRef]) => Linker.link(sym, root).invoke(args)
      val as = args.map(visitVarSym)
      Term.Head.App(f, as)
  }

  def visitBodyTerm(t: ExecutableAst.Term.Body)(implicit root: ExecutableAst.Root, flix: Flix): Term.Body = t match {
    case ExecutableAst.Term.Body.Wild(_, _) => Term.Body.Wild()
    case ExecutableAst.Term.Body.Var(sym, _, _) => Term.Body.Var(visitVarSym(sym))
    case ExecutableAst.Term.Body.Lit(lit, _, _) => Term.Body.Lit(() => lit)
    case ExecutableAst.Term.Body.Cst(sym, _, _) => Term.Body.Cst(() => Linker.link(sym, root).invoke(Array.emptyObjectArray))
    case ExecutableAst.Term.Body.Pat(_, _, _) => throw new UnsupportedOperationException("Loop currently not supported")
  }

  def visitVarSym(s: Symbol.VarSym)(implicit root: ExecutableAst.Root, flix: Flix): VarSym = VarSym(s.id, s.getStackOffset)

  def visitAttribute(a: ExecutableAst.Attribute)(implicit root: ExecutableAst.Root, flix: Flix): Attribute =
    Attribute(a.name)

  def visitLatOps(tables: Map[Symbol.TableSym, ExecutableAst.Table])(implicit root: ExecutableAst.Root, flix: Flix): Map[TableSym, LatticeOps] = {
    tables.foldLeft(Map.empty[TableSym, LatticeOps]) {
      case (macc, (sym, ExecutableAst.Table.Relation(_, attributes, _))) =>
        // relation
        macc

      case (macc, (sym, ExecutableAst.Table.Lattice(_, keys, value, _))) =>
        // lattice

        val lattice = root.lattices(value.tpe)

        val latticeOps = new LatticeOps {
          override def bot: ProxyObject = Linker.link(lattice.bot, root).invoke(Array.empty)

          override def equ: InvocationTarget = Linker.link(lattice.equ, root)

          override def leq: InvocationTarget = Linker.link(lattice.leq, root)

          override def lub: InvocationTarget = Linker.link(lattice.lub, root)

          override def glb: InvocationTarget = Linker.link(lattice.glb, root)
        }
        macc + (visitTableSym(sym) -> latticeOps)
    }
  }

}
