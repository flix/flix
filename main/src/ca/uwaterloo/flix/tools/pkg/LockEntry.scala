/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.util.Sha256

/**
  * What one Flix package was, as of the last time it was downloaded.
  *
  * A package is downloaded as two files, and each one gets its own digest: a change to the
  * `.toml` and a change to the `.fpkg` are different events, and the two are worth telling apart
  * when one of them turns out to be unexpected.
  *
  * An entry does not carry the identifier of the package it describes. A [[Lockfile]] holds its
  * entries by identifier, so carrying it here would be a second place for it to disagree.
  *
  * An entry is written as one table in the lock file, under the identifier of its package:
  *
  * {{{
  * [packages."github:flix/museum"]
  * version = "1.2.3"
  * toml    = "sha256:d440e50454f31af3176813e02ea68ef786e4d3cea27d26934b484e73cf575dca"
  * fpkg    = "sha256:d6ba2b0aee0ca923732881584d8c4fa2815d2802827283e0ad84173581569969"
  * }}}
  *
  * @param version the version of the package.
  * @param toml    the digest of the `flix.toml` of the package.
  * @param fpkg    the digest of the `.fpkg` of the package.
  */
case class LockEntry(version: SemVer, toml: Sha256, fpkg: Sha256)
