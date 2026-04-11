///
/// Portable character sets currently supported across JVM, llvm-native, and llvm-wasm.
///
pub enum CharacterSet {

    case Utf8

}

pub mod CharacterSet {

    ///
    /// Returns the UTF-8 character set.
    ///
    pub def utf8(): CharacterSet = CharacterSet.Utf8

}
