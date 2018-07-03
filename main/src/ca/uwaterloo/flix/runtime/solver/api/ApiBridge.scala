package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, ExecutableAst, Symbol}
import ca.uwaterloo.flix.runtime.{InvocationTarget, Linker}
import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.api.predicate._
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym
import ca.uwaterloo.flix.runtime.solver.api.term._

import scala.collection.mutable

object ApiBridge {

  // TODO: Transitory class.
  // TODO: Accept arguments only as proxy objects?

  // Class used to ensure that the symbols share the same object by identity.
  class SymbolCache {

    val varSyms = mutable.Map.empty[Symbol.VarSym, VarSym]
    val relSyms = mutable.Map.empty[Symbol.TableSym, Relation]
    val latSyms = mutable.Map.empty[Symbol.TableSym, Lattice]

    def getVarSym(sym: Symbol.VarSym): VarSym =
      varSyms.get(sym) match {
        case None =>
          val newSym = new VarSym()
          varSyms += (sym -> newSym)
          newSym
        case Some(res) => res
      }

    def getRelSym(sym: Symbol.TableSym, name: String, attributes: Array[Attribute]): Relation =
      relSyms.get(sym) match {
        case None =>
          val newSym = new Relation(name, attributes)
          relSyms += (sym -> newSym)
          newSym
        case Some(res) => res
      }

    def getLatSym(sym: Symbol.TableSym, name: String, keys: Array[Attribute], value: Attribute, ops: LatticeOps): Lattice =
      latSyms.get(sym) match {
        case None =>
          val newSym = new Lattice(name, keys, value, ops)
          latSyms += (sym -> newSym)
          newSym
        case Some(res) => res
      }

  }

  def translate(root: ExecutableAst.Root)(implicit flix: Flix): ConstraintSet = {
    implicit val _ = root
    implicit val cache = new SymbolCache

    val strata = root.strata.map(visitStratum)

    val relSyms = cache.relSyms.values.toArray
    val latSyms = cache.latSyms.values.toArray

    new ConstraintSet(relSyms, latSyms, strata)
  }

  private def visitStratum(stratum: ExecutableAst.Stratum)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Stratum = {
    new Stratum(stratum.constraints.map(visitConstraint))
  }

  private def visitConstraint(c0: ExecutableAst.Constraint)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Constraint = {
    val cparams = c0.cparams.map(visitConstraintParam)
    val head = visitHeadPredicate(c0.head)
    val body = c0.body.map(visitBodyPredicate)
    new Constraint(cparams, head, body)
  }

  private def visitConstraintParam(c0: ExecutableAst.ConstraintParam)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): VarSym = c0 match {
    case ExecutableAst.ConstraintParam.HeadParam(sym, _, _) => visitVarSym(sym)
    case ExecutableAst.ConstraintParam.RuleParam(sym, _, _) => visitVarSym(sym)
  }

  private def visitHeadPredicate(h0: ExecutableAst.Predicate.Head)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Predicate = h0 match {
    case ExecutableAst.Predicate.Head.True(_) => new TruePredicate()
    case ExecutableAst.Predicate.Head.False(_) => new FalsePredicate()
    case ExecutableAst.Predicate.Head.Atom(sym, terms, _) => new AtomPredicate(visitTableSym(sym), positive = true, terms.map(visitHeadTerm).toArray, null)
  }

  private def visitBodyPredicate(b0: ExecutableAst.Predicate.Body)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Predicate = b0 match {
    case ExecutableAst.Predicate.Body.Atom(sym, polarity, terms, index2sym, loc) =>
      val s = visitTableSym(sym)
      val p = polarity match {
        case Ast.Polarity.Positive => true
        case Ast.Polarity.Negative => false
      }
      val ts = terms.map(visitBodyTerm)
      val i2s = index2sym map {
        case x if x != null => visitVarSym(x)
        case _ => null
      }
      new AtomPredicate(s, p, ts.toArray, i2s.toArray)

    case ExecutableAst.Predicate.Body.Filter(sym, terms, _) =>
      val f = (as: Array[AnyRef]) => Linker.link(sym, root).invoke(as).getValue.asInstanceOf[Boolean].booleanValue()
      val ts = terms.map(visitBodyTerm)
      new FilterPredicate(f, ts.toArray)

    case ExecutableAst.Predicate.Body.Loop(_, _, _) => throw new UnsupportedOperationException("Loop currently not supported")
  }

  private def visitTableSym(sym: Symbol.TableSym)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Table =
    root.tables(sym) match {
      case r: ExecutableAst.Table.Relation =>
        val attributes = r.attributes.map(visitAttribute)
        cache.getRelSym(sym, sym.toString, attributes)

      case l: ExecutableAst.Table.Lattice =>
        val keys = l.keys.map(visitAttribute)
        val value = visitAttribute(l.value)
        val ops = getLatticeOps(l.value)
        cache.getLatSym(sym, sym.toString, keys, value, ops)
    }

  private def visitHeadTerm(t0: ExecutableAst.Term.Head)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Term = t0 match {
    case ExecutableAst.Term.Head.Var(sym, _, _) => new VarTerm(visitVarSym(sym))
    case ExecutableAst.Term.Head.Lit(lit, _, _) => new LitTerm(() => lit)
    case ExecutableAst.Term.Head.Cst(sym, _, _) => new LitTerm(() => Linker.link(sym, root).invoke(Array.emptyObjectArray))
    case ExecutableAst.Term.Head.App(sym, args, _, _) =>
      val f = (args: Array[AnyRef]) => Linker.link(sym, root).invoke(args)
      val as = args.map(visitVarSym)
      new AppTerm(f, as.toArray)
  }

  def visitBodyTerm(t0: ExecutableAst.Term.Body)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Term = t0 match {
    case ExecutableAst.Term.Body.Wild(_, _) => new WildTerm()
    case ExecutableAst.Term.Body.Var(sym, _, _) => new VarTerm(visitVarSym(sym))
    case ExecutableAst.Term.Body.Lit(lit, _, _) => new LitTerm(() => lit)
    case ExecutableAst.Term.Body.Cst(sym, _, _) => new LitTerm(() => Linker.link(sym, root).invoke(Array.emptyObjectArray))
    case ExecutableAst.Term.Body.Pat(_, _, _) => throw new UnsupportedOperationException("Loop currently not supported")
  }

  private def visitVarSym(sym: Symbol.VarSym)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): VarSym =
    cache.getVarSym(sym)

  private def visitAttribute(a: ExecutableAst.Attribute)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Attribute =
    new Attribute(a.name)

  private def visitLatOps(tables: Map[Symbol.TableSym, ExecutableAst.Table])(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): Map[Table, LatticeOps] = {
    tables.foldLeft(Map.empty[Table, LatticeOps]) {
      case (macc, (sym, ExecutableAst.Table.Relation(_, attributes, _))) =>
        // relation
        macc

      case (macc, (sym, ExecutableAst.Table.Lattice(_, keys, value, _))) =>
        // lattice
        val latticeOps = getLatticeOps(value)
        macc + (visitTableSym(sym) -> latticeOps)
    }
  }

  private def getLatticeOps(value: ExecutableAst.Attribute)(implicit root: ExecutableAst.Root, cache: SymbolCache, flix: Flix): LatticeOps = {
    val lattice = root.lattices(value.tpe)

    new LatticeOps {
      override def bot: ProxyObject = Linker.link(lattice.bot, root).invoke(Array.empty)

      override def equ: InvocationTarget = Linker.link(lattice.equ, root)

      override def leq: InvocationTarget = Linker.link(lattice.leq, root)

      override def lub: InvocationTarget = Linker.link(lattice.lub, root)

      override def glb: InvocationTarget = Linker.link(lattice.glb, root)
    }
  }

}
