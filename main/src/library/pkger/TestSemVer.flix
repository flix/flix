use SemVer.{isCompatible};
use SemVer.SemVer;

@test
def testHigherPatchInSpecificWhenZeroMajor(): Bool = isCompatible(SemVer.SemVer(0, 1, 2), SemVer.SemVer(0, 1, 0))

@test
def testHigherPatchInBoundWhenZeroMajor(): Bool = isCompatible(SemVer.SemVer(0, 1, 0), SemVer.SemVer(0, 1, 2))

@test
def testSamePatchWhenZeroMajor(): Bool = isCompatible(SemVer.SemVer(0, 1, 0), SemVer.SemVer(0, 1, 2))

@test
def testLargerMinorFailsWhenZeroMajor(): Bool = !isCompatible(SemVer.SemVer(0, 2, 1), SemVer.SemVer(0, 1, 0))

@test
def testSmallerMinorFailsWhenZeroMajor(): Bool = !isCompatible(SemVer.SemVer(0, 1, 1), SemVer.SemVer(0, 2, 0))

@test
def testLargerMajorFailsWhenZeroMajor(): Bool = !isCompatible(SemVer.SemVer(1, 0, 0), SemVer.SemVer(0, 1, 0))

@test
def testLargerPatchWhenNonZeroMajor(): Bool = isCompatible(SemVer.SemVer(1, 2, 3), SemVer.SemVer(1, 2, 0))

@test
def testSmallerPatchWhenNonZeroMajor(): Bool = isCompatible(SemVer.SemVer(1, 2, 0), SemVer.SemVer(1, 2, 3))
