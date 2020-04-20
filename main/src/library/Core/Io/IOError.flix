namespace Core/Io/IOError {
    ///
    /// Represents an error when operating in an I/O context.
    ///
    pub enum IOError {
        // Generalized
        case FileNotFound
        case IncorrectBufferSize

        // Exceptions
        case IOException
        case ZipException
        case IllegalStateException
        case NoSuchElementException
    }
}