namespace SemVer {
    ///
    /// Represents an instance of a Semantic Versioning (SemVer) version.
    /// It is currently missing pre-release identifiers and build metadata
    ///
    pub enum SemVer {
        case SemVer(Int, Int, Int)
    }


    ///
    /// Compare two SemVer instances according to the ordering rules in the specification
    ///
    def __cmp(l: SemVer, r: SemVer): Int32 = match (l, r) {
        case (SemVer(lMajor, lMinor, lPatch), SemVer(rMajor, rMinor, rPatch)) => {
            if (lMajor == rMajor) {
                if (lMinor == rMinor) {
                    if (lPatch == rPatch) {
                        0
                    } else if (lPatch > rPatch) {
                        1
                    } else {
                        -1
                    }
                } else if (lMajor > rMajor) {
                    1
                } else {
                    -1
                }
            } else if (lMajor > rMajor) {
                1
            } else {
                -1
            }
        }
    }

    ///
    /// Check whether the `specific` SemVer is compatible with the `bound` SemVer
    ///
    pub def isCompatible(bound: SemVer, specific: SemVer): Bool = {
        match (bound, specific) {
            case (SemVer(0, boundMinor, _), SemVer(0, specificMinor, _)) => {
                boundMinor == specificMinor
            }
            case (SemVer(boundMajor, _, _), SemVer(specificMajor, _, _)) => boundMajor == specificMajor
        }
    }

    namespace Parser {
        def parseNumber(): Parser[Int] = Parser.mapOption(Parser.failOn(Parser.span(Char.isDigit), String.isEmpty), s -> Result.toOption(Int32.fromString(s)))

        ///
        /// A parser reading a string into the SemVer enum
        ///
        pub def parseSemVer(): Parser[SemVer.SemVer] = Parser.bind(parseNumber(), major -> {
            Parser.bind(Parser.char('.') *> parseNumber(), minor -> {
                Parser.bind(Parser.char('.') *> parseNumber(), patch -> {
                    use SemVer.SemVer;
                    Parser.return(SemVer.SemVer(major, minor, patch))
                })
            })
        })
    }
}
