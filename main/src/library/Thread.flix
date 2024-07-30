/*
 * Copyright 2022 Magnus Madsen
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
mod Thread {
    import java.lang.Thread
    import java.lang.Long

    use Time.Duration
    use Time.Duration.Duration

    ///
    /// Sleeps for the given duration `d`.
    ///
    pub def sleep(d: Duration): Unit \ IO =
        import java.lang.Long.intValue(): Int32 \ {};
        let Duration(nsTotal) = d;
        let ms = nsTotal / 1_000_000i64;
        let ns: Int32 = if (nsTotal > 0i64) intValue(Long.valueOf($INT64_REM$(nsTotal, 1_000_000i64))) else 0;
        Thread.sleep(ms, ns)

}
