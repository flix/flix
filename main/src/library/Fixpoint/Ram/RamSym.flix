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


mod Fixpoint.Ram {

    use Fixpoint.PredSymsOf
    use Fixpoint.SubstitutePredSym
    use Fixpoint.Ast.Denotation
    use Fixpoint.Shared.PredSym

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
}
