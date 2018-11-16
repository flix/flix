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


///////////////////////////////////////////////////////////////////////////////
/// Float32                                                                 ///
///////////////////////////////////////////////////////////////////////////////
rel Float32Rel(x: Float32)

@test
def testFloat32Lit01(): Unit =
    let x = Float32Rel(0.0f32).;
    let r = solve x;
    ()

@test
def testFloat32Lit02(): Unit =
    let x = Float32Rel(-123.456f32).;
    let r = solve x;
    ()


@test
def testFloat32Lit03(): Unit =
    let x = Float32Rel(123.567f32).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Float64                                                                 ///
///////////////////////////////////////////////////////////////////////////////
rel Float64Rel(x: Float64)

@test
def testFloat64Lit01(): Unit =
    let x = Float64Rel(0.0f64).;
    let r = solve x;
    ()

@test
def testFloat64Lit02(): Unit =
    let x = Float64Rel(-123.456f64).;
    let r = solve x;
    ()


@test
def testFloat64Lit03(): Unit =
    let x = Float64Rel(123.567f64).;
    let r = solve x;
    ()



