use Pkger/SemVer.{isCompatible, SemVer};
use Pkger/SemVer.SemVer.SemVer;
use Pkger/SemVer/Parser.fromString;

@test
def testHigherPatchInSpecificWhenZeroMajor01(): Bool = not isCompatible(SemVer.SemVer(0, 1, 2), SemVer.SemVer(0, 1, 0))

@test
def testHigherPatchInSpecificWhenZeroMajor02(): Bool = not isCompatible(SemVer.SemVer(0, 1, 10), SemVer.SemVer(0, 1, 0))

@test
def testHigherPatchInSpecificWhenZeroMajor03(): Bool = not isCompatible(SemVer.SemVer(0, 1, 256), SemVer.SemVer(0, 1, 27))

@test
def testHigherPatchInSpecificWhenZeroMajor04(): Bool = not isCompatible(SemVer.SemVer(0, 22, 13), SemVer.SemVer(0, 22, 12))

@test
def testHigherPatchInBoundWhenZeroMajor01(): Bool = not isCompatible(SemVer.SemVer(0, 1, 5), SemVer.SemVer(0, 1, 10))

@test
def testHigherPatchInBoundWhenZeroMajor02(): Bool = not isCompatible(SemVer.SemVer(0, 1, 71), SemVer.SemVer(0, 1, 72))

@test
def testHigherPatchInBoundWhenZeroMajor03(): Bool = not isCompatible(SemVer.SemVer(0, 1, 333), SemVer.SemVer(0, 1, 666))

@test
def testHigherPatchInBoundWhenZeroMajor04(): Bool = not isCompatible(SemVer.SemVer(0, 1, 13), SemVer.SemVer(0, 1, 42))

@test
def testSamePatchWhenZeroMajor01(): Bool = isCompatible(SemVer.SemVer(0, 1, 10), SemVer.SemVer(0, 1, 10))

@test
def testSamePatchWhenZeroMajor02(): Bool = isCompatible(SemVer.SemVer(0, 1, 14), SemVer.SemVer(0, 1, 14))

@test
def testSamePatchWhenZeroMajor03(): Bool = isCompatible(SemVer.SemVer(0, 1, 62), SemVer.SemVer(0, 1, 62))

@test
def testSamePatchWhenZeroMajor04(): Bool = isCompatible(SemVer.SemVer(0, 1, 42), SemVer.SemVer(0, 1, 42))

@test
def testLargerMinorFailsWhenZeroMajor01(): Bool = not isCompatible(SemVer.SemVer(0, 2, 1), SemVer.SemVer(0, 1, 0))

@test
def testLargerMinorFailsWhenZeroMajor02(): Bool = not isCompatible(SemVer.SemVer(0, 2, 2), SemVer.SemVer(0, 1, 2))

@test
def testLargerMinorFailsWhenZeroMajor03(): Bool = not isCompatible(SemVer.SemVer(0, 42, 42), SemVer.SemVer(0, 41, 0))

@test
def testLargerMinorFailsWhenZeroMajor04(): Bool = not isCompatible(SemVer.SemVer(0, 10, 1), SemVer.SemVer(0, 7, 200))

@test
def testSmallerMinorFailsWhenZeroMajor01(): Bool = not isCompatible(SemVer.SemVer(0, 1, 1), SemVer.SemVer(0, 2, 0))

@test
def testSmallerMinorFailsWhenZeroMajor02(): Bool = not isCompatible(SemVer.SemVer(0, 1, 1), SemVer.SemVer(0, 2, 0))

@test
def testSmallerMinorFailsWhenZeroMajor03(): Bool = not isCompatible(SemVer.SemVer(0, 1, 1), SemVer.SemVer(0, 2, 0))

@test
def testSmallerMinorFailsWhenZeroMajor04(): Bool = not isCompatible(SemVer.SemVer(0, 1, 1), SemVer.SemVer(0, 2, 0))

@test
def testLargerMajorFailsWhenZeroMajor01(): Bool = not isCompatible(SemVer.SemVer(1, 0, 0), SemVer.SemVer(0, 1, 0))

@test
def testLargerMajorFailsWhenZeroMajor02(): Bool = not isCompatible(SemVer.SemVer(52, 7, 2), SemVer.SemVer(0, 1, 5))

@test
def testLargerMajorFailsWhenZeroMajor03(): Bool = not isCompatible(SemVer.SemVer(6, 1, 7), SemVer.SemVer(0, 1, 4))

@test
def testLargerMajorFailsWhenZeroMajor04(): Bool = not isCompatible(SemVer.SemVer(4, 0, 0), SemVer.SemVer(0, 1, 87))

@test
def testLargerPatchWhenNonZeroMajor01(): Bool = isCompatible(SemVer.SemVer(1, 2, 3), SemVer.SemVer(1, 2, 0))

@test
def testLargerPatchWhenNonZeroMajor02(): Bool = isCompatible(SemVer.SemVer(1, 2, 3), SemVer.SemVer(1, 2, 0))

@test
def testLargerPatchWhenNonZeroMajor03(): Bool = isCompatible(SemVer.SemVer(1, 2, 3), SemVer.SemVer(1, 2, 0))

@test
def testLargerPatchWhenNonZeroMajor04(): Bool = isCompatible(SemVer.SemVer(1, 2, 3), SemVer.SemVer(1, 2, 0))

@test
def testSmallerPatchWhenNonZeroMajor01(): Bool = isCompatible(SemVer.SemVer(1, 2, 0), SemVer.SemVer(1, 2, 3))

@test
def testSmallerPatchWhenNonZeroMajor02(): Bool = isCompatible(SemVer.SemVer(1, 2, 0), SemVer.SemVer(1, 2, 3))

@test
def testSmallerPatchWhenNonZeroMajor03(): Bool = isCompatible(SemVer.SemVer(1, 2, 0), SemVer.SemVer(1, 2, 3))

@test
def testSmallerPatchWhenNonZeroMajor04(): Bool = isCompatible(SemVer.SemVer(1, 2, 0), SemVer.SemVer(1, 2, 3))

@test
def parseSemVer01(): Bool = fromString("1.2.3") == Some(SemVer(1, 2, 3))

@test
def parseSemVer02(): Bool = fromString("0.1.2") == Some(SemVer(0, 1, 2))

@test
def parseSemVer03(): Bool = fromString("10.52.64") == Some(SemVer(10, 52, 64))

@test
def parseSemVer04(): Bool = fromString("0.4") == None

@test
def parseSemVer05(): Bool = fromString("4") == None

@test
def parseSemVer06(): Bool = fromString("10.4") == None
