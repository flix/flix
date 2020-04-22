namespace Core/Io/IOError {
    ///
    /// Represents an error when operating in an I/O context.
    ///
    pub enum IOError {
        // Generalized
        case ElementNotFound
        case FileNotFound
        case Generic
        case IllegalState
        case IncorrectBufferSize
        case MalformedZip
    }
}