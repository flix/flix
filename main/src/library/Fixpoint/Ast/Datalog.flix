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

use Fixpoint/Ram.RamSym;
use Fixpoint.Tuple;
use Fixpoint.Tuple.Tuple;

namespace Fixpoint/Ast {
    pub enum Datalog[v] {
        case Datalog(Array[Constraint[v]], Array[Constraint[v]])
        case Model(Map[RamSym[v], Map[Tuple[v], v]])
        case Join(Datalog[v], Datalog[v])
    }

    instance ToString[Datalog[v]] with ToString[v] {
        pub def toString(cs: Datalog[v]): String = match cs {
            case Datalog(facts, rules) =>
                let sb = StringBuilder.new();
                StringBuilder.appendLinesWith!(ToString.toString, facts, sb);
                StringBuilder.appendLinesWith!(ToString.toString, rules, sb);
                StringBuilder.toString(sb)
            case Model(db) =>
                use Fixpoint/Ram.toDenotation;
                use Fixpoint.commaSeparate;
                let sb = StringBuilder.new();
                Map.foreach(ramSym -> rel -> match toDenotation(ramSym) {
                    case Relational => Map.foreach(match Tuple(tuple) -> _ -> {
                        StringBuilder.appendLine!("${ramSym}(${commaSeparate(tuple)}).", sb)
                    }, rel)
                    case Latticenal(_) => Map.foreach(match Tuple(tuple) -> lat -> {
                        StringBuilder.appendLine!("${ramSym}(${commaSeparate(tuple)}; ${lat}).", sb)
                    }, rel)
                }, db);
                StringBuilder.toString(sb)
            case Join(d1, d2) =>
                let sb = StringBuilder.new();
                StringBuilder.appendLine!(ToString.toString(d1), sb);
                StringBuilder.appendLine!(ToString.toString(d2), sb);
                StringBuilder.toString(sb)
        } as & Pure
    }
}
