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

mod Fixpoint.Ram {

    @Internal
    pub enum RowVar with Eq {
        case Named(String)
        case Index(Int32)
    }

    instance ToString[RowVar] {
        pub def toString(var: RowVar): String = match var {
            case RowVar.Named(name) => name
            case RowVar.Index(i) => "${i}$"
        }
    }

    instance Order[RowVar] {
        pub def compare(a: RowVar, b: RowVar): Comparison = match (a, b) {
            case (RowVar.Named(s1), RowVar.Named(s2)) => s1 <=> s2
            case (RowVar.Index(i1), RowVar.Index(i2)) => i1 <=> i2
            case _ => ?bug
        }
    }
}
