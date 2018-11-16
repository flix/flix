///////////////////////////////////////////////////////////////////////////////
/// Unit                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel UnitRel(x: Unit)

@test
def testUnitLit01(): Unit =
    let x = UnitRel(()).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Bool                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel BoolRel(x: Bool)

@test
def testBoolLit01(): Unit =
    let x = BoolRel(true).;
    let r = solve x;
    ()

@test
def testBoolLit02(): Unit =
    let x = BoolRel(false).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Char                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel CharRel(x: Char)

@test
def testCharLit01(): Unit =
    let x = CharRel(' ').;
    let r = solve x;
    ()

@test
def testCharLit02(): Unit =
    let x = CharRel('a').;
    let r = solve x;
    ()

@test
def testCharLit03(): Unit =
    let x = CharRel('x').;
    let r = solve x;
    ()


