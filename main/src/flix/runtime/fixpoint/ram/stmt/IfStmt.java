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

import flix.runtime.fixpoint.ram.exp.bool.BoolExp;

import java.io.PrintStream;

public final class IfStmt implements Stmt {
    private final BoolExp guard;
    private final Stmt bodyStmt;

    public IfStmt(BoolExp guard, Stmt bodyStmt) {
        if (guard == null) throw new IllegalArgumentException("'boolExp' must be non-null");
        if (bodyStmt == null) throw new IllegalArgumentException("'stmt' must be non-null");
        this.guard = guard;
        this.bodyStmt = bodyStmt;
    }

    public BoolExp getGuard() {
        return guard;
    }

    public Stmt getBodyStmt() {
        return bodyStmt;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel));
        stream.print("if (");
        guard.prettyPrint(stream, indentLevel);
        stream.print(") then {\n");
        bodyStmt.prettyPrint(stream, indentLevel + 1);
        stream.print("\n" + "\t".repeat(indentLevel) + "}");
    }
}
