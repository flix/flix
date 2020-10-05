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

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;
import flix.runtime.fixpoint.ram.exp.relation.TableName;

import java.io.PrintStream;

public final class AssignStmt implements Stmt {
    private final TableName name;
    private final RelationExp relationExp;

    public AssignStmt(TableName name, RelationExp relationExp) {
        if (name == null) throw new IllegalArgumentException("'name' must be non-null");
        if (relationExp == null) throw new IllegalArgumentException("'relationExp' must be non-null");
        this.name = name;
        this.relationExp = relationExp;
    }

    public RelationExp getRelationExp() {
        return relationExp;
    }

    public TableName getName() {
        return name;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel));
        name.prettyPrint(stream, indentLevel);
        stream.print(" := ");
        relationExp.prettyPrint(stream, indentLevel);
    }
}
