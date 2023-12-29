/*
 * Copyright 2021 Benjamin Dahse
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod Fixpoint.Ast.Ram {

    use Fixpoint.Ast.Denotation
    use Fixpoint.Shared.PredSym
    use Fixpoint.PredSymsOf
    use Fixpoint.SubstitutePredSym

    /////////////////////////////////////////////////////////////////////////////
    // RamStmt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum RamStmt[v] {
        case Insert(RelOp[v])
        case Merge(RamSym[v], RamSym[v])
        case Assign(RamSym[v], RamSym[v])
        case Purge(RamSym[v])
        case Seq(Vector[RamStmt[v]])
        case Until(Vector[BoolExp[v]], RamStmt[v])
        case Comment(String)
    }

    instance ToString[RamStmt[v]] {
        pub def toString(stmt: RamStmt[v]): String =
            let nl = String.lineSeparator();
            match stmt {
                case RamStmt.Insert(op) => ToString.toString(op)
                case RamStmt.Merge(src, dst) => "merge ${src} into ${dst}"
                case RamStmt.Assign(lhs, rhs) => "${lhs} := ${rhs}"
                case RamStmt.Purge(ramSym) => "purge ${ramSym}"
                case RamStmt.Seq(xs) => Vector.join(";${nl}", xs)
                case RamStmt.Until(test, body) =>
                    let tst = test |> Vector.join(" Λ ");
                    "until(${tst}) do${nl}${String.indent(4, "${body}")}end"
                case RamStmt.Comment(comment) => "// ${comment}"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RelOp                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum RelOp[v] {
        case Search(RowVar, RamSym[v], RelOp[v])
        case Query(RowVar, RamSym[v], Vector[(Int32, RamTerm[v])], RelOp[v])
        case Functional(RowVar, Vector[v] -> Vector[Vector[v]], Vector[RamTerm[v]], RelOp[v])
        case Project(Vector[RamTerm[v]], RamSym[v])
        case If(Vector[BoolExp[v]], RelOp[v])
    }

    instance ToString[RelOp[v]] {
        pub def toString(op: RelOp[v]): String =
            let nl = String.lineSeparator();
            match op {
                case RelOp.Search(var, ramSym, body) =>
                    "search ${var} ∈ ${ramSym} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Query(var, ramSym, prefixQuery, body) =>
                    let query = Vector.joinWith(match (i, term) -> {
                        ToString.toString(BoolExp.Eq(RamTerm.RowLoad(var, i), term))
                    }, " ∧ ", prefixQuery);
                    "query {${var} ∈ ${ramSym} | ${query}} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Functional(rowVar, _, terms, body) =>
                    "loop(${rowVar} <- f(${terms |> Vector.join(", ")})) do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Project(terms, ramSym) =>
                    "project (${terms |> Vector.join(", ")}) into ${ramSym}"
                case RelOp.If(test, then) =>
                    let tst = test |> Vector.join(" ∧ ");
                    "if(${tst}) then${nl}${String.indent(4, "${then}")}end"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // BoolExp                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum BoolExp[v] {
        case Empty(RamSym[v])
        case NotMemberOf(Vector[RamTerm[v]], RamSym[v])
        case Eq(RamTerm[v], RamTerm[v])
        case Leq(v -> v -> Bool, RamTerm[v], RamTerm[v])
        case Guard0(Unit -> Bool)
        case Guard1(v -> Bool, RamTerm[v])
        case Guard2(v -> v -> Bool, RamTerm[v], RamTerm[v])
        case Guard3(v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard4(v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard5(v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
    }

    instance ToString[BoolExp[v]] {
        pub def toString(exp: BoolExp[v]): String =
            match exp {
                case BoolExp.Empty(ramSym) => "${ramSym} == ∅"
                case BoolExp.NotMemberOf(terms, ramSym) => "(${terms |> Vector.join(", ")}) ∉ ${ramSym}"
                case BoolExp.Eq(lhs, rhs) => "${lhs} == ${rhs}"
                case BoolExp.Leq(_, lhs, rhs) => "${lhs} ≤ ${rhs}"
                case BoolExp.Guard0(_) => "<clo>()"
                case BoolExp.Guard1(_, v) => "<clo>(${v})"
                case BoolExp.Guard2(_, v1, v2) => "<clo>(${v1}, ${v2})"
                case BoolExp.Guard3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
                case BoolExp.Guard4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case BoolExp.Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RamTerm                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Represents a RAM term.
    ///
    /// `RowLoad(x, i)` represents an index into the tuple
    /// bound to the local variable `x`, i.e. `x[i]`.
    ///
    /// `LoadLatVar(x)` represents the lattice component in the tuple
    /// bound to the local variable `x`.
    ///
    @Internal
    pub enum RamTerm[v] {
        case Lit(v)
        case RowLoad(RowVar, Int32)
        case LoadLatVar(RowVar)
        case Meet(v -> v -> v, RamTerm[v], RamTerm[v])
        case App0(Unit -> v)
        case App1(v -> v, RamTerm[v])
        case App2(v -> v -> v, RamTerm[v], RamTerm[v])
        case App3(v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v])
        case App4(v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App5(v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
    }

    instance ToString[RamTerm[v]] {
        pub def toString(term: RamTerm[v]): String = match term {
            case RamTerm.Lit(v) => "%{v}"
            case RamTerm.RowLoad(var, index) => "${var}[${index}]"
            case RamTerm.LoadLatVar(var) => "${var}[-1]"
            case RamTerm.Meet(_, lhs, rhs) => "(${lhs} ⊓ ${rhs})"
            case RamTerm.App0(_) => "<clo>()"
            case RamTerm.App1(_, v) => "<clo>(${v})"
            case RamTerm.App2(_, v1, v2) => "<clo>(${v1}, ${v2})"
            case RamTerm.App3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
            case RamTerm.App4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case RamTerm.App5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RamSym                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum RamSym[v] {
        case Full(PredSym, Int32, Denotation[v])
        case Delta(PredSym, Int32, Denotation[v])
        case New(PredSym, Int32, Denotation[v])
    }

    instance Eq[RamSym[v]] {
        pub def eq(a: RamSym[v], b: RamSym[v]): Bool = match (a, b) {
            case (RamSym.Full(s1, _, _),  RamSym.Full(s2, _, _))  => s1 == s2
            case (RamSym.Delta(s1, _, _), RamSym.Delta(s2, _, _)) => s1 == s2
            case (RamSym.New(s1, _, _),   RamSym.New(s2, _, _))   => s1 == s2
            case _                                  => false
        }
    }

    instance Order[RamSym[v]] {
        pub def compare(a: RamSym[v], b: RamSym[v]): Comparison = match a {
            case RamSym.Full(s1, _, _) => match b {
                case RamSym.Full(s2, _, _) => s1 <=> s2
                case _ => Comparison.GreaterThan
            }
            case RamSym.Delta(s1, _, _) => match b {
                case RamSym.Delta(s2, _, _) => s1 <=> s2
                case RamSym.New(_) => Comparison.GreaterThan
                case RamSym.Full(_) => Comparison.LessThan
            }
            case RamSym.New(s1, _, _) => match b {
                case RamSym.New(s2, _, _) => s1 <=> s2
                case _ => Comparison.LessThan
            }
        }
    }

    instance PredSymsOf[RamSym[v]] {
        pub def predSymsOf(x: RamSym[v]): Set[PredSym] = match x {
            case RamSym.Full(predSym, _, _)    => Set.singleton(predSym)
            case RamSym.Delta(predSym, _, _)   => Set.singleton(predSym)
            case RamSym.New(predSym, _, _)     => Set.singleton(predSym)
        }
    }

    instance SubstitutePredSym[RamSym[v]] {
        pub def substitute(x: RamSym[v], s: Map[PredSym, PredSym]): RamSym[v] = match x {
            case RamSym.Full(predSym, arity, den)  =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                RamSym.Full(newSym, arity, den)

            case RamSym.Delta(predSym, arity, den) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                RamSym.Delta(newSym, arity, den)

            case RamSym.New(predSym, arity, den)   =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                RamSym.New(newSym, arity, den)
        }
    }

    instance ToString[RamSym[v]] {
        pub def toString(ramSym: RamSym[v]): String = match ramSym {
            case RamSym.Full(sym, _, _)    => "${sym}"
            case RamSym.Delta(sym, _, _)   => "Δ${sym}"
            case RamSym.New(sym, _, _)     => "Δ${sym}'"
        }
    }

    @Internal
    pub def arityOf(ramSym: RamSym[v]): Int32 = match ramSym {
        case RamSym.Full(_, arity, _) => arity
        case RamSym.Delta(_, arity, _) => arity
        case RamSym.New(_, arity, _) => arity
    }

    @Internal
    pub def toDenotation(ramSym: RamSym[v]): Denotation[v] = match ramSym {
        case RamSym.Full(_, _, den) => den
        case RamSym.Delta(_, _, den) => den
        case RamSym.New(_, _, den) => den
    }

    /////////////////////////////////////////////////////////////////////////////
    // RowVar                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum RowVar with Eq {
        case Named(String)
        case Index(Int32)
    }

    instance ToString[RowVar] {
        pub def toString(var: RowVar): String = match var {
            case RowVar.Named(name) => name
            case RowVar.Index(i) => "${i}$"
        }
    }

    instance Order[RowVar] {
        pub def compare(a: RowVar, b: RowVar): Comparison = match (a, b) {
            case (RowVar.Named(s1), RowVar.Named(s2)) => s1 <=> s2
            case (RowVar.Index(i1), RowVar.Index(i2)) => i1 <=> i2
            case _ => ?bug
        }
    }

}
