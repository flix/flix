/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
pub mod Object {

    import java.util.Objects
    import java.lang.Object

    ///
    /// Returns `true` if the given value `x` is `null`.
    ///
    @Inline
    pub def isNull(x: a): Bool =
        match Reflect.reflectValue(x) {
            case Reflect.JvmValue.JvmObject(obj) => Objects.isNull(obj)
            case _                               => false
        }

    ///
    /// Returns `None` if the given value `x` is `null`, otherwise returns `Some(x)`.
    ///
    pub def toOption(x: a): Option[a] =
        if (isNull(x)) None else Some(x)

}
