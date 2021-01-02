/*
 * Copyright 2020 Esben Bjerre
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

use Core/Cmp.Ordering;
use Core/Cmp.Ordering.{LessThan, Equal, GreaterThan};

instance Eq[Ordering] {

    def eq(x: Ordering, y: Ordering): Bool = match (x, y) {
        case (LessThan, LessThan) => true
        case (Equal, Equal) => true
        case (GreaterThan, GreaterThan) => true
        case _ => false
    }

    def neq(x: Ordering, y: Ordering): Bool = not (x == y)

}

///
/// An ordering type.
///
namespace Core/Cmp {

    pub enum Ordering {
        case LessThan
        case Equal
        case GreaterThan
    }

}