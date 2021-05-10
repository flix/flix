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

    pub enum PredSym {
        case PredSym(String)
    }

    instance Order[PredSym] {
        pub def compare(a: PredSym, b: PredSym): Comparison = match a {
            case PredSym(s1) => match b {
                case PredSym(s2) => s1 <=> s2
            }
        }
    }

    instance Eq[PredSym] {
        pub def eq(x: PredSym, y: PredSym): Bool = match x {
            case PredSym(s1) => match y {
                case PredSym(s2) => s1 == s2
            }
        }
    }

    instance ToString[PredSym] {
        pub def toString(predSym: PredSym): String = match predSym {
            case PredSym(name) => name
        }
    }

}
