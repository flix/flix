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

package flix.runtime.fixpoint.ram.exp.bool;

import java.io.PrintStream;

public class OrBoolExp implements BoolExp {

    private final BoolExp leftExp;
    private final BoolExp rightExp;

    public OrBoolExp(BoolExp leftExp, BoolExp rightExp) {
        this.leftExp = leftExp;
        this.rightExp = rightExp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        leftExp.prettyPrint(stream, indentLevel);
        stream.print("||");
        rightExp.prettyPrint(stream, indentLevel);
    }

    public BoolExp getLeftExp() {
        return leftExp;
    }

    public BoolExp getRightExp() {
        return rightExp;
    }
}
