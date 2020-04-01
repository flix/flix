namespace SemVer {
    // TODO: Consider adding pre-release identifier and build metadata
    pub type alias SemVer = (Int, Int, Int)

    pub enum Ord {
        case Gt
        case Eq
        case Lt
    }

    pub def isWithinBounds(specific: SemVer, bound: SemVer): Bool = {
        let majorIsZero = match bound {
            case (major, _, _) => major == 0
        };

        if (!majorIsZero) {
            match (specific, bound) {
                // Only major versions allow backwards incompatibility
                case ((major, _, _), (boundMajor, _, _)) => boundMajor == major
            }
        } else {
            // TODO: Look only for specific version, or allow patch releases? Only specific for now!
            match (specific, bound) {
                case ((major, minor, _), (boundMajor, boundMinor, _)) => major == boundMajor && minor == boundMinor
            }
        }
    }

    pub def cmpSemVer(left: SemVer, right: SemVer): Ord = match (left, right) {
        case ((lMajor, lMinor, lPatch), (rMajor, rMinor, rPatch)) => {
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
            Parser.bind(Parser.char('.') *> parseNumber(), minor -> {
                Parser.bind(Parser.char('.') *> parseNumber(), patch -> {
                    Parser.return((major, minor, patch))
                })
            })
        })
    }

    namespace Test {
        def ppBool(b: Bool): String = match b {
            case true => "true"
            case false => "false"
        }

        def test(name: String, f: () -> Bool): Bool & Impure = {
            Console.print("Testing ... ");
            Console.print(name);
            Console.print(": ");
            let res = f();
            Console.printLine(ppBool(res));
            res
        }

        def testZeroMajorIsPatchCompatible(): Bool & Impure = {
            use SemVer.isWithinBounds;

            let higherPatchInSpecific = test("higherPatchInSpecific", () -> isWithinBounds((0, 1, 2), (0, 1, 0)));
            let higherPatchInBound = test("higherPatchInBound", () -> isWithinBounds((0, 1, 0), (0, 1, 2)));
            let samePatch = test("higherPatchInBound", () -> isWithinBounds((0, 1, 0), (0, 1, 2)));

            higherPatchInSpecific && higherPatchInBound && samePatch
        }

        def testZeroMajorIsMinorIncompatible(): Bool & Impure = {
            use SemVer.isWithinBounds;

            let largerMinorFails = test("largerMinorFails", () -> !isWithinBounds((0, 2, 1), (0, 1, 0)));
            let smallerMinorFails = test("smallerMinorFails", () -> !isWithinBounds((0, 1, 1), (0, 2, 0)));

            largerMinorFails && smallerMinorFails
        }

        pub def allTests(): Bool & Impure = {
            testZeroMajorIsPatchCompatible() && testZeroMajorIsMinorIncompatible()
        }
    }
}
