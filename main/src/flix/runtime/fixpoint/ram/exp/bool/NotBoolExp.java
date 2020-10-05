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

public final class NotBoolExp implements BoolExp {
    private final BoolExp exp;

    public NotBoolExp(BoolExp exp) {
        if (exp == null) throw new IllegalArgumentException("'epx' must be non-null");
        this.exp = exp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("Not (");
        exp.prettyPrint(stream, indentLevel);
        stream.print(')');
    }

    public BoolExp getExp() {
        return exp;
    }
}
