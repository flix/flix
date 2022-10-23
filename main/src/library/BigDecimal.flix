/*
 * Copyright 2022 Stephen Tetley
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

namespace BigDecimal {

    ///
    /// Parse the string `s` as an BigDecimal, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[BigDecimal] = try {
        import java.lang.String.strip(): String \ {};
        import new java.math.BigDecimal(String): BigDecimal \ {} as parseBigDecimal;
        Some(s |> strip |> parseBigDecimal)
    } catch {
        case _: ##java.lang.NumberFormatException => None
    }

}
