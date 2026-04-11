///
/// JVM-only extensions for `Char`.
///

pub mod Char {

    import java.lang.Character

    ///
    /// Get the primitive Char value from its object representation, i.e., java.lang.Character.
    ///
    /// This function is expected to be used when marshaling Chars from Java. Generally in Flix
    /// code you should not need to use `java.lang.Character`.
    ///
    pub def charValue(c: Character): Char =
        c.charValue()

    ///
    /// Convert an Char value to its its object representation (i.e. java.lang.Character).
    ///
    /// This function is expected to be used when marshaling Chars to Java. Generally in Flix
    /// code you should not need to use `java.lang.Character`.
    ///
    pub def valueOf(c: Char): Character =
        Character.valueOf(c)

}

