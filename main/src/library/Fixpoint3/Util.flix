/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fixpoint3.Util {
    ///
    /// Unwraps an option.
    ///
    /// Causes runtime hole error when `x` is `None`.
    ///
    pub def getOrCrash(x: Option[a]): a = match x {
        case Some(v) => v
        case None    => bug!("Attempted unwrapping none in getOrCrash")
    }

}
