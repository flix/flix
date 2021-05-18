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
namespace Fixpoint/Ast {
    pub enum VarSym {
        case VarSym(String)
    }

    instance Eq[VarSym] {
        pub def eq(a: VarSym, b: VarSym): Bool = Fixpoint/Ast/VarSym.compare(a, b) == EqualTo
    }

    instance Order[VarSym] {
        pub def compare(a: VarSym, b: VarSym): Comparison = Fixpoint/Ast/VarSym.compare(a, b)
    }

    instance ToString[VarSym] {
        pub def toString(varSym: VarSym): String = match varSym {
            case VarSym(name) => name
        }
    }

    namespace VarSym {
        use Fixpoint/Ast.VarSym;
        use Fixpoint/Ast.VarSym.VarSym;

        pub def compare(a: VarSym, b: VarSym): Comparison = match (a, b) {
            case (VarSym(s1), VarSym(s2)) => s1 <=> s2
        }
    }
}
