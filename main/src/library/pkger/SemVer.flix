namespace SemVer {
    // TODO: Consider adding pre-release identifier and build metadata
    pub enum SemVer {
        case SemVer(Int, Int, Int)
    }

    def __cmp(l: SemVer, r: SemVer): Int32 = {
        match cmpSemVer(l, r) {
            case Gt => 1
            case Eq => 0
            case Lt => -1
        }
    }

    pub enum Ord {
        case Gt
        case Eq
        case Lt
    }

    pub def isWithinBounds(bound: SemVer, specific: SemVer): Bool = {
        match (bound, specific) {
            case (SemVer(0, boundMinor, boundPatch), SemVer(0, specificMinor, specificPatch)) => {
                boundMinor == specificMinor && boundPatch == specificPatch
            }
            case (SemVer(boundMajor, _, _), SemVer(specificMajor, _, _)) => boundMajor == specificMajor
        }
    }

    pub def cmpSemVer(left: SemVer, right: SemVer): Ord = match (left, right) {
        case (SemVer(lMajor, lMinor, lPatch), SemVer(rMajor, rMinor, rPatch)) => {
            if (lMajor == rMajor) {
                if (lMinor == rMinor) {
                    if (lPatch == rPatch) {
                        Eq
                    } else if (lPatch > rPatch) {
                        Gt
                    } else {
                        Lt
                    }
                } else if (lMajor > rMajor) {
                    Gt
                } else {
                    Lt
                }
            } else if (lMajor > rMajor) {
                Gt
            } else {
                Lt
            }
        }
    }

    pub def ppOrd(o: Ord): String = match o {
        case Gt => "Gt",
        case Eq => "Eq",
        case Lt => "Lt"
    }

    namespace Parser {
        def parseNumber(): Parser[Int] = Parser.mapOption(Parser.failOn(Parser.span(Char.isDigit), String.isEmpty), s -> Result.toOption(Int32.fromString(s)))

        pub def parseSemVer(): Parser[SemVer.SemVer] = Parser.bind(parseNumber(), major -> {
            use SemVer.SemVer;

            Parser.bind(Parser.char('.') *> parseNumber(), minor -> {
                Parser.bind(Parser.char('.') *> parseNumber(), patch -> {
                    Parser.return(SemVer.SemVer(major, minor, patch))
                })
            })
        })
    }

    namespace Test {
        def testZeroMajorIsPatchCompatible(): Bool & Impure = {
            use SemVer.{isWithinBounds, SemVer};
            use Test.test;

            let higherPatchInSpecific = test("higherPatchInSpecific", () -> isWithinBounds(SemVer.SemVer(0, 1, 2), SemVer.SemVer(0, 1, 0)));
            let higherPatchInBound = test("higherPatchInBound", () -> isWithinBounds(SemVer.SemVer(0, 1, 0), SemVer.SemVer(0, 1, 2)));
            let samePatch = test("higherPatchInBound", () -> isWithinBounds(SemVer.SemVer(0, 1, 0), SemVer.SemVer(0, 1, 2)));

            higherPatchInSpecific && higherPatchInBound && samePatch
        }

        def testZeroMajorIsMinorIncompatible(): Bool & Impure = {
            use SemVer.{isWithinBounds, SemVer};
            use Test.test;

            let largerMinorFails = test("largerMinorFails", () -> !isWithinBounds(SemVer.SemVer(0, 2, 1), SemVer.SemVer(0, 1, 0)));
            let smallerMinorFails = test("smallerMinorFails", () -> !isWithinBounds(SemVer.SemVer(0, 1, 1), SemVer.SemVer(0, 2, 0)));

            largerMinorFails && smallerMinorFails
        }

        def testZeroMajorIsMajorIncompatible(): Bool & Impure = {
            use SemVer.{isWithinBounds, SemVer};
            use Test.test;

            let largerMajorFails = test("largerMajorFails", () -> !isWithinBounds(SemVer.SemVer(1, 0, 0), SemVer.SemVer(0, 1, 0)));

            largerMajorFails
        }

        def testNonZeroMajorIsPatchCompatible(): Bool & Impure = {
            use SemVer.{isWithinBounds, SemVer};
            use Test.test;

            let largerPatch = test("largerPatch", () -> isWithinBounds(SemVer.SemVer(1, 2, 3), SemVer.SemVer(1, 2, 0)));
            let smallerPatch = test("smallerPatch", () -> isWithinBounds(SemVer.SemVer(1, 2, 0), SemVer.SemVer(1, 2, 3)));
            largerPatch && smallerPatch
        }

        pub def allTests(): Bool & Impure = {
            testZeroMajorIsPatchCompatible()
                && testZeroMajorIsMinorIncompatible()
                && testZeroMajorIsMajorIncompatible()
                && testNonZeroMajorIsPatchCompatible()
        }
    }
}
