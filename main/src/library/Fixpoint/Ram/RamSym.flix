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

use Fixpoint.PredSymsOf;
use Fixpoint.SubstitutePredSym;
use Fixpoint/Ast.Denotation;
use Fixpoint/Shared.PredSym;

namespace Fixpoint/Ram {
    pub enum RamSym[v] {
        case Full(PredSym, Int32, Denotation[v])
        case Delta(PredSym, Int32, Denotation[v])
        case New(PredSym, Int32, Denotation[v])
    }

    instance Eq[RamSym[v]] {
        pub def eq(a: RamSym[v], b: RamSym[v]): Bool = match (a, b) {
            case (Full(s1, _, _),  Full(s2, _, _))  => s1 == s2
            case (Delta(s1, _, _), Delta(s2, _, _)) => s1 == s2
            case (New(s1, _, _),   New(s2, _, _))   => s1 == s2
            case _                                  => false
        }
    }

    instance Order[RamSym[v]] {
        pub def compare(a: RamSym[v], b: RamSym[v]): Comparison = match a {
            case Full(s1, _, _) => match b {
                case Full(s2, _, _) => s1 <=> s2
                case _ => GreaterThan
            }
            case Delta(s1, _, _) => match b {
                case Delta(s2, _, _) => s1 <=> s2
                case New(_) => GreaterThan
                case Full(_) => LessThan
            }
            case New(s1, _, _) => match b {
                case New(s2, _, _) => s1 <=> s2
                case _ => LessThan
            }
        }
    }

    instance PredSymsOf[RamSym[v]] {
        pub def predSymsOf(x: RamSym[v]): Set[PredSym] = match x {
            case Full(predSym, _, _)    => Set.singleton(predSym)
            case Delta(predSym, _, _)   => Set.singleton(predSym)
            case New(predSym, _, _)     => Set.singleton(predSym)
        }
    }

    instance SubstitutePredSym[RamSym[v]] {
        pub def substitute(x: RamSym[v], s: Map[PredSym, PredSym]): RamSym[v] = match x {
            case Full(predSym, arity, den)  =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                Full(newSym, arity, den)

            case Delta(predSym, arity, den) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                Delta(newSym, arity, den)

            case New(predSym, arity, den)   =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                New(newSym, arity, den)
        }
    }

    instance ToString[RamSym[v]] {
        pub def toString(ramSym: RamSym[v]): String = match ramSym {
            case Full(sym, _, _)    => "${sym}"
            case Delta(sym, _, _)   => "Δ${sym}"
            case New(sym, _, _)     => "Δ${sym}'"
        }
    }

    @Internal
    pub def arityOf(ramSym: RamSym[v]): Int32 = match ramSym {
        case Full(_, arity, _) => arity
        case Delta(_, arity, _) => arity
        case New(_, arity, _) => arity
    }

    @Internal
    pub def toDenotation(ramSym: RamSym[v]): Denotation[v] = match ramSym {
        case Full(_, _, den) => den
        case Delta(_, _, den) => den
        case New(_, _, den) => den
    }
}
