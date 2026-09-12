/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.MoveOption {

    ///
    /// Options for the `moveWith` operation.
    ///
    pub enum MoveOption with Eq, Order, ToString {
        case AtomicMove
        case ReplaceExisting
    }

}
