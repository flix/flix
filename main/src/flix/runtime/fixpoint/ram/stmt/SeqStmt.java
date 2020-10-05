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

import java.io.PrintStream;
import java.util.ArrayList;

/**
 * The statement "stmt1; stmt2"
 */
public final class SeqStmt implements Stmt {
    private final Stmt[] stmts;

    public SeqStmt(Stmt[] stmts) {
        if (stmts == null || stmts.length < 1)
            throw new IllegalArgumentException("'stmts' must be non-null and non-empty");
        this.stmts = stmts;
    }

    public SeqStmt(ArrayList<Stmt> stmts) {
        if (stmts == null || stmts.size() < 1)
            throw new IllegalArgumentException("'stmts' must be non-null and non-empty");
        this.stmts = stmts.toArray(Stmt[]::new);
    }

    public Stmt[] getStmts() {
        return stmts.clone();
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        for (int i = 0; i < stmts.length; i++) {
            Stmt stmt = stmts[i];
            if (stmt == null) {
                stream.println("PrettyPrint failed in SeqStmt at: " + (i + 1) + "\n but there was actually: " + stmts.length);
                return;
            }
            stmt.prettyPrint(stream, indentLevel);
            if (i < stmts.length - 1) {
                stream.print(";\n");
            }
        }
    }
}
