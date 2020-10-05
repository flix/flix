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

package flix.runtime.fixpoint.ram.exp.relation;

import java.io.PrintStream;

public class UnionRelationExp implements RelationExp {
    private final RelationExp exp1;
    private final RelationExp exp2;

    public UnionRelationExp(RelationExp exp1, RelationExp exp2) {
        this.exp1 = exp1;
        this.exp2 = exp2;
    }

    public RelationExp getExp1() {
        return exp1;
    }

    public RelationExp getExp2() {
        return exp2;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        exp1.prettyPrint(stream, indentLevel);
        stream.print(" U ");
        exp2.prettyPrint(stream, indentLevel);
    }
}
