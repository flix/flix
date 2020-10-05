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

import flix.runtime.fixpoint.ram.RowVariable;
import flix.runtime.fixpoint.ram.exp.relation.TableName;

import java.io.PrintStream;

public final class ForEachStmt implements Stmt {
    private final TableName name;
    private final RowVariable localVar;
    private final Stmt body;

    public ForEachStmt(TableName name, RowVariable localVar, Stmt body) {
        if (name == null) throw new IllegalArgumentException("'name' must be non-null");
        if (localVar == null) throw new IllegalArgumentException("'localVar' must be non-null");
        if (body == null) throw new IllegalArgumentException("'body' must be non-null");
        this.name = name;
        this.localVar = localVar;
        this.body = body;
    }

    public TableName getName() {
        return name;
    }

    public RowVariable getLocalVar() {
        return localVar;
    }

    public Stmt getBody() {
        return body;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel));
        stream.print("for each ");
        name.prettyPrint(stream, indentLevel);
        stream.print(" as " +
                localVar.getVarName() + " do {\n");
        body.prettyPrint(stream, indentLevel + 1);
        stream.print('\n' + "\t".repeat(indentLevel) + '}');
    }
}
