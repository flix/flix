/*
 *  Copyright 2022 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

mod MonadZero {

    ///
    /// A trait for Monads that have a zero element.
    ///
    trait MonadZero[m: Type -> Type] with Monad[m] {

        ///
        /// Returns the zero element of the monad.
        ///
        pub def empty(): m[t]

    }

}
