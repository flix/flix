mod TestReflection {
    ///
    /// Represents a zero-width position within a source, identified by line and column coordinates.
    ///
    /// Both line and column numbers use one-based indexing.
    /// (e.g., the first character of a source is at line 1, column 1).
    ///
    /// Fields:
    ///
    /// 1. `line`: The line number (1-based). Must be >= 1.
    /// 2. `column`: The column number (1-based) within the specified line. Must be >= 1.
    ///
    pub enum SourcePosition with Coerce, ToString {
        case SourcePosition(Int32, Int32)
    }

    ///
    /// Represents a span or range within a source, defined by a source identifier and
    /// start/end source positions.
    ///
    /// Fields:
    ///
    /// 1. `source`: The source identifier (typically a file path)
    /// 2. `start`: The beginning position of the span (inclusive)
    /// 3. `end`: The ending position of the span (inclusive)
    ///
    pub enum SourceLocation with Coerce, ToString {
        case SourceLocation(String, SourcePosition, SourcePosition)
    }

    ///
    /// Creates a `SourceLocation` from a source identifier and raw line/column coordinates.
    ///
    /// Returns a `SourceLocation` spanning from (`line1`, `col1`) to (`line2`, `col2`) in the given source.
    ///
    pub def mkSourceLocation(source: String, line1: Int32, col1: Int32, line2: Int32, col2: Int32) : SourceLocation =
        use SourceLocation.SourceLocation;
        use SourcePosition.SourcePosition;
        SourceLocation(
                source,
                SourcePosition(line1, col1),
                SourcePosition(line2, col2)
        )

    ///
    /// Represents a unit test case with associated metadata.
    ///
    /// A unit test contains a name, source location information, and a test function
    /// that can be either pure (only Assert effect) or effectful (Assert + IO effects).
    ///
    pub enum UnitTest {
        case TestPure ({name = String, sourceLocation = TestReflection.SourceLocation, fn = Unit -> Unit \ Assert})
        case TestWithIO ({name = String, sourceLocation = TestReflection.SourceLocation, fn = Unit -> Unit \ Assert + IO})
    }

    instance ToString[UnitTest] {
        pub def toString(t: UnitTest): String =
            match t {
                case UnitTest.TestPure(r) =>
                    "TestPure(name = ${r#name}, sourceLocation = ${r#sourceLocation})"

                case UnitTest.TestWithIO(r) =>
                    "TestWithIO(name = ${r#name}, sourceLocation = ${r#sourceLocation})"
            }
    }

    ///
    /// Creates a unit test with IO effects.
    ///
    /// Constructs a `UnitTest` that can perform IO operations in addition to assertions.
    /// The source location is specified using explicit line and column coordinates.
    ///
    pub def mkIOUnitTest(name: String, source: String, line1: Int32, col1: Int32, line2: Int32, col2: Int32, fn: Unit -> Unit \ Assert + IO) : UnitTest =
        UnitTest.TestWithIO({name=name, sourceLocation=TestReflection.mkSourceLocation(source, line1, col1, line2, col2), fn = fn})

    ///
    /// Creates a pure unit test.
    ///
    /// Constructs a `UnitTest` that only performs assertions without side effects.
    /// The source location is specified using explicit line and column coordinates.
    ///
    pub def mkPureUnitTest(name: String, source: String, line1: Int32, col1: Int32, line2: Int32, col2: Int32, fn: Unit -> Unit \ Assert) : UnitTest =
        UnitTest.TestPure({name=name, sourceLocation=TestReflection.mkSourceLocation(source, line1, col1, line2, col2), fn = fn})

    ///
    /// Returns the name of the given unit test.
    ///
    pub def getUnitTestName(test: UnitTest): String = match test {
        case UnitTest.TestPure(rec) => rec#name
        case UnitTest.TestWithIO(rec) => rec#name
    }

    ///
    /// Returns the source location of the given unit test.
    ///
    pub def getUnitTestSourceLocation(test: UnitTest): TestReflection.SourceLocation = match test {
        case UnitTest.TestPure(rec) => rec#sourceLocation
        case UnitTest.TestWithIO(rec) => rec#sourceLocation
    }

    ///
    /// Returns the test function of the given unit test.
    ///
    /// The returned function is cast to have both Assert and IO,
    /// regardless of whether the original test was pure or effectful, as that
    /// is the only sound approximate.
    ///
    pub def getUnitTestFn(test: UnitTest): Unit -> Unit \ Assert + IO = match test {
        case UnitTest.TestPure(rec) => unchecked_cast(rec#fn as Unit -> Unit \ {Assert + IO})
        case UnitTest.TestWithIO(rec) => rec#fn
    }

    ///
    /// Returns `true` if the given unit test is pure (has no IO effects).
    ///
    pub def isUnitTestPure(test: UnitTest): Bool = match test {
        case UnitTest.TestPure(_) => true
        case UnitTest.TestWithIO(_) => false
    }
}
