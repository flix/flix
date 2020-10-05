/*
 * Copyright 2020 Andreas Salling Heglingegård
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package flix.runtime.fixpoint.ram.stmt;

import flix.runtime.fixpoint.ram.exp.relation.TableName;
import flix.runtime.fixpoint.ram.term.RamTerm;

import java.io.PrintStream;

public final class ProjectStmt implements Stmt {
    private final RamTerm[] facts;
    private final TableName table;

    public ProjectStmt(RamTerm[] terms, TableName table) {
        if (terms == null)
            throw new IllegalArgumentException("'terms' must be non-null");
        if (table == null) throw new IllegalArgumentException("'table' must be non-null");
        this.facts = terms;
        this.table = table;
    }

    public RamTerm[] getFacts() {
        return facts.clone();
    }

    public TableName getTable() {
        return table;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel) +
                "project (");
        for (int i = 0; i < facts.length; i++) {
            RamTerm fact = facts[i];
            fact.prettyPrint(stream);
            if (i < facts.length - 1) {
                stream.print(", ");
            }
        }
        stream.print(") into ");
        table.prettyPrint(stream, indentLevel);
    }
}
