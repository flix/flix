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

public final class WhileStmt implements Stmt {
    private final BoolExp condition;
    private final Stmt body;

    public WhileStmt(BoolExp condition, Stmt body) {
        if (condition == null) throw new IllegalArgumentException("'condition' must be non-null");
        if (body == null) throw new IllegalArgumentException("'body' must be non.null");
        this.condition = condition;
        this.body = body;
    }

    public BoolExp getCondition() {
        return condition;
    }

    public Stmt getBody() {
        return body;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("\t".repeat(indentLevel) + "while (");
        condition.prettyPrint(stream, indentLevel);
        stream.print("){\n");
        body.prettyPrint(stream, indentLevel + 1);
        stream.print('\n' + "\t".repeat(indentLevel) + '}');
    }
}
