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

instance Order[BigDecimal] {

    redef less(x: BigDecimal, y: BigDecimal): Bool =
        x.compareTo(y) < 0

    redef lessEqual(x: BigDecimal, y: BigDecimal): Bool =
        x.compareTo(y) <= 0

    redef greater(x: BigDecimal, y: BigDecimal): Bool =
        x.compareTo(y) > 0

    redef greaterEqual(x: BigDecimal, y: BigDecimal): Bool =
        x.compareTo(y) >= 0

    pub def compare(x: BigDecimal, y: BigDecimal): Comparison =
        match x.compareTo(y) {
            case 0          => Comparison.EqualTo
            case z if z < 0 => Comparison.LessThan
            case _          => Comparison.GreaterThan
        }

}

instance Order[BigInt] {

    redef less(x: BigInt, y: BigInt): Bool =
        x.compareTo(y) < 0

    redef lessEqual(x: BigInt, y: BigInt): Bool =
        x.compareTo(y) <= 0

    redef greater(x: BigInt, y: BigInt): Bool =
        x.compareTo(y) > 0

    redef greaterEqual(x: BigInt, y: BigInt): Bool =
        x.compareTo(y) >= 0

    pub def compare(x: BigInt, y: BigInt): Comparison =
        match x.compareTo(y) {
            case 0          => Comparison.EqualTo
            case z if z < 0 => Comparison.LessThan
            case _          => Comparison.GreaterThan
        }

}
