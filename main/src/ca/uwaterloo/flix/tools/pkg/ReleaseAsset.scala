/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Bootstrap

/**
  * One of the two files a package publishes with every release: its manifest and its package.
  *
  * Both are published under a fixed name, so the address of either follows from the repository,
  * the version, and the name alone, and is read at the cost of one request that either finds the
  * file or does not. The release listing is never read to find one, which matters because it
  * costs a request against the API rate limit: 60 an hour for an anonymous client, shared by
  * every package a build resolves.
  */
sealed trait ReleaseAsset {

  /** The name the file is published under, e.g. `flix.toml`. */
  def name: String

  /** The extension the file is cached under in `lib/`, e.g. `toml`. */
  def extension: String

}

object ReleaseAsset {

  /** The package itself, published as `package.fpkg`. */
  case object Fpkg extends ReleaseAsset {
    val name: String = Bootstrap.PACKAGE_FPKG
    val extension: String = Bootstrap.EXT_FPKG
  }

  /** The manifest of the package, published as `flix.toml`. */
  case object Toml extends ReleaseAsset {
    val name: String = Bootstrap.FLIX_TOML
    val extension: String = Bootstrap.EXT_TOML
  }

}
