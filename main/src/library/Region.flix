/*
 * Copyright 2023 Paul Butcher
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

mod Region {

  ///
  /// Add x to a stack of `Closeable` to be closed when the region exits:
  ///
  /// * `Closeable`s are guaranteed to be closed after any threads within the
  ///   region have been joined.
  /// * `Closeable`s are guaranteed to be closed in the reverse order in
  ///   which they're added (most recently added first).
  ///
  pub def closeOnExit(x: a, r: Region[r]): Unit \ {r, IO} with Closeable[a] =
      $SCOPE_EXIT$(() -> Closeable.close(x), r)
}
